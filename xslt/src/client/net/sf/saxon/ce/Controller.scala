// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce

import client.net.sf.saxon.ce.dom.HTMLWriter
import client.net.sf.saxon.ce.event._
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.functions.Component
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.lib.ErrorListener
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.lib.StandardErrorListener
import client.net.sf.saxon.ce.lib.TraceListener
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.Mode
import client.net.sf.saxon.ce.trans.RuleManager
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.trans.update.PendingUpdateList
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.tree.linked.LinkedTreeBuilder
import client.net.sf.saxon.ce.value.DateTimeValue
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.dom.client.Node
import com.google.gwt.logging.client.LogConfiguration
import com.google.gwt.user.client.Event
import java.io.PrintStream
import java.util.ArrayList
import java.util.Date
import java.util.HashMap
import java.util.HashSet
import Controller._

import scala.beans.BeanProperty


object Controller {

  private var eventProcessors: ArrayList[Xslt20ProcessorImpl] = null

  private var nonDomEventProcessors: ArrayList[Xslt20ProcessorImpl] = null

  /**
   * Create a Controller and initialise variables. Note: XSLT applications should
   * create the Controller by using the JAXP newTransformer() method, or in S9API
   * by using XsltExecutable.load()
   *
   * @param proc The processor
   */
  def addEventProcessor(proc: Xslt20ProcessorImpl): Unit = {
    eventProcessors = addProcessor(eventProcessors, proc)
  }

  def relayEvent(node: Node, event: Event): Unit = {
    if (eventProcessors != null) {
      for (p <- eventProcessors if p != null) {
        p.bubbleApplyTemplates(node, event)
      }
    }
  }

  def addProcessor(list: ArrayList[Xslt20ProcessorImpl], proc: Xslt20ProcessorImpl): ArrayList[Xslt20ProcessorImpl] = {
    if (list == null) {
      list = new ArrayList[Xslt20ProcessorImpl]()
    }
    list.add(proc)
    list
  }

  def addNonDomEventProcessor(proc: Xslt20ProcessorImpl): Unit = {
    nonDomEventProcessors = addProcessor(nonDomEventProcessors, proc)
  }

  def relayNonDomEvent(name: String, target: JavaScriptObject, event: JavaScriptObject): Unit = {
    if (nonDomEventProcessors != null) {
      for (p <- nonDomEventProcessors if p != null) {
        val sqn = new StructuredQName("", NamespaceConstant.IXSL, name)
        p.applyEventTemplates(sqn.getClarkName, null, event, target)
      }
    }
  }

  object APIcommand extends Enumeration {

    val UPDATE_HTML = new APIcommand()

    val TRANSFORM_TO_DOCUMENT = new APIcommand()

    val TRANSFORM_TO_FRAGMENT = new APIcommand()

    val TRANSFORM_TO_HTML_FRAGMENT = new APIcommand()

    val NONE = new APIcommand()

    class APIcommand extends Val

    implicit def convertValue(v: Value): APIcommand = v.asInstanceOf[APIcommand]
  }

  /**
   * Returns a JavaScript object with a property for each result document.
   * The property name is the URI and the property value the document object
   * @param keys A JavaScript array of result document keys (URIs)
   * @return A JavaScript object with similar functionality to a HashMap.
   */
  private /* native */ def createArray(keys: JavaScriptObject): JavaScriptObject
}

/**
 * The Controller is equivalent to Saxon-HE's implementation of the same name, and represents
 * an executing instance of a transformation or query. Multiple concurrent executions of
 * the same transformation or query will use different Controller instances. This class is
 * therefore not thread-safe.
 * <p>
 * The Controller is serially reusable, when one transformation or query
 * is finished, it can be used to run another. However, there is no advantage in doing this
 * rather than allocating a new Controller each time. An inert version of the controller can
 * be used to simply hold state for the benefit of the JavaScript API, controller settings
 * can then be copied to a 'live' Controller instance using importControllerSettings()
 * <p>
 * A dummy Controller is created by the JavaScript API for holding settings.
 * <p>
 * The Controller holds those parts of the dynamic context that do not vary during the course
 * of a transformation or query, or that do not change once their value has been computed.
 * This also includes those parts of the static context that are required at run-time.
 * <p>
 *
 * @author Michael H. Kay
 * @since 8.4
 */
class Controller {

  private var config: Configuration = _

  @BeanProperty
  var initialContextItem: Item = _

  @BeanProperty
  var contextForGlobalVariables: Item = _

  @BeanProperty
  var bindery: Bindery = _

  @BeanProperty
  var ruleManager: RuleManager = _

  @BeanProperty
  var parameters: HashMap[StructuredQName, Sequence] = _

  private var principalResultURI: String = _

  @BeanProperty
  var errorListener: ErrorListener = _

  @BeanProperty
  var executable: Executable = _

  private var initialTemplate: Template = null

  private var allOutputDestinations: HashSet[DocumentURI] = _

  private var resultDocumentPool: HashMap[DocumentURI, Node] = _

  private var reusableSequenceOutputter: SequenceOutputter = null

  private var userDataTable: HashMap[String, Any] = new HashMap[String, Any](20)

  @BeanProperty
  lazy val currentDateTime = DateTimeValue.fromJavaDate(new Date())

  private var dateTimePreset: Boolean = false

  private var initialMode: StructuredQName = null

  private var lastRememberedNode: NodeInfo = null

  private var lastRememberedNumber: Int = -1

  private var inUse: Boolean = false

  private var stripSourceTrees: Boolean = true

  @BeanProperty
  var pendingUpdateList: PendingUpdateList = _

  @BeanProperty
  var initialTemplateName: String = null

  private var isInert: Boolean = _

  @BeanProperty
  var targetNode: Node = _

  private var commandType: APIcommand = _

  private var openHTMLWriter: HTMLWriter = null

  private var principalOutputNode: Node = null

  @BeanProperty
  var sourceNode: NodeInfo = null

  def this(config: Configuration, isInert: Boolean) {
    this()
    this.isInert = isInert
    this.config = config
    executable = new Executable(config)
    reset()
  }

  def this(config: Configuration) {
    this()
    isInert = false
    this.config = config
    executable = new Executable(config)
    reset()
  }

  /**
   * Create a Controller and initialise variables.
   *
   * @param config The Configuration used by this Controller
   * @param executable The executable used by this Controller
   */
  def this(config: Configuration, executable: Executable) {
    this()
    isInert = false
    this.config = config
    this.executable = executable
    this.errorListener = config.getErrorListener
    reset()
  }

  /**
   * <p>Reset this <code>Transformer</code> to its original configuration.</p>
   * <p/>
   * <p><code>Transformer</code> is reset to the same state as when it was created with
   * [[javax.xml.transform.TransformerFactory#newTransformer()]],
   * [[javax.xml.transform.TransformerFactory#newTransformer(javax.xml.transform.Source source)]] or
   * [[javax.xml.transform.Templates#newTransformer()]].
   * <code>reset()</code> is designed to allow the reuse of existing <code>Transformer</code>s
   * thus saving resources associated with the creation of new <code>Transformer</code>s.</p>
   * <p>
   * <p>The reset <code>Transformer</code> is not guaranteed to have the same [[javax.xml.transform.URIResolver]]
   * or [[javax.xml.transform.ErrorListener]] <code>Object</code>s, e.g. [[Object#equals(Object obj)]].
   * It is guaranteed to have a functionally equal <code>URIResolver</code>
   * and <code>ErrorListener</code>.</p>
   *
   * @since 1.5
   */
  def reset(): Unit = {
    bindery = new Bindery()
    if (errorListener.isInstanceOf[StandardErrorListener]) {
      val ps = errorListener.asInstanceOf[StandardErrorListener].getErrorOutput
      errorListener = errorListener.asInstanceOf[StandardErrorListener].makeAnother()
      errorListener.asInstanceOf[StandardErrorListener].setErrorOutput(ps)
    }
    contextForGlobalVariables = null
    parameters = null
    currentDateTime = null
    dateTimePreset = false
    initialContextItem = null
    initialMode = null
    initialTemplate = null
    initialTemplateName = null
    clearPerTransformationData()
    pendingUpdateList = new PendingUpdateList(config)
    targetNode = null
    commandType = APIcommand.NONE
    resultDocumentPool = null
    openHTMLWriter = null
  }

  def importControllerSettings(lc: Controller): Unit = {
    this.setBaseOutputURI(lc.getBaseOutputURI)
    this.setInitialMode(lc.getInitialMode)
    this.setInitialTemplate(lc.getInitialTemplateName)
    this.setParameters(lc.getParameters)
    this.setBaseOutputURI(lc.getBaseOutputURI)
    this.setTargetNode(lc.getTargetNode)
    this.setApiCommand(lc.getApiCommand)
    this.setSourceNode(lc.getSourceNode)
  }

  /**
   * Reset variables that need to be reset for each transformation if the controller
   * is serially reused
   */
  private def clearPerTransformationData(): Unit = {
    allOutputDestinations = null
    resultDocumentPool = null
    lastRememberedNode = null
    lastRememberedNumber = -1
    openHTMLWriter = null
  }

  /**
   * Get the Configuration associated with this Controller. The Configuration holds
   * settings that potentially apply globally to many different queries and transformations.
   * @return the Configuration object
   * @since 8.4
   */
  def getConfiguration(): Configuration = config

  def setApiCommand(command: APIcommand): Unit = {
    commandType = command
  }

  def getApiCommand(): APIcommand = commandType

  /**
   * Set the initial mode for the transformation.
   * <p>
   * XSLT 2.0 allows a transformation to be started in a mode other than the default mode.
   * The transformation then starts by looking for the template rule in this mode that best
   * matches the initial context node.
   * <p>
   * This method may eventually be superseded by a standard JAXP method.
   *
   * @param expandedModeName the name of the initial mode.  The mode is
   *     supplied as an expanded QName, that is "localname" if there is no
   *     namespace, or "{uri}localname" otherwise. If the value is null or zero-length,
   *     the initial mode is reset to the unnamed default mode.
   * @since 8.4
   */
  def setInitialMode(expandedModeName: String): Unit = {
    initialMode = if (expandedModeName == null || expandedModeName.length == 0) null else StructuredQName.fromClarkName(expandedModeName)
  }

  /**
   * Get the initial mode for the transformation
   * @return the initial mode, as a name in Clark format
   */
  def getInitialMode(): String = {
    if (initialMode == null) {
      null
    } else {
      initialMode.getClarkName
    }
  }

  /**
   * Set the base output URI.
   *
   * <p>This defaults to the system ID of the Result object for the principal output
   * of the transformation if this is known; if it is not known, it defaults
   * to the current directory.</p>
   *
   * <p> The base output URI is used for resolving relative URIs in the <code>href</code> attribute
   * of the <code>xsl:result-document</code> instruction.</p>
   
   *
   * @param uri the base output URI
   * @since 8.4
   */
  def setBaseOutputURI(uri: String): Unit = {
    principalResultURI = uri
  }

  /**
   * Get the base output URI.
   *
   * <p>This returns the value set using the [[#setBaseOutputURI]] method. If no value has been set
   * explicitly, then the method returns null if called before the transformation, or the computed
   * default base output URI if called after the transformation.</p>
   *
   * <p> The base output URI is used for resolving relative URIs in the <code>href</code> attribute
   * of the <code>xsl:result-document</code> instruction.</p>
   *
   * @return the base output URI
   * @since 8.4
   */
  def getBaseOutputURI(): String = principalResultURI

  /**
   * Check that an output destination has not been used before, optionally adding
   * this URI to the set of URIs that have been used.
   * @param uri the URI to be used as the output destination
   * @return true if the URI is available for use; false if it has already been used.
   * <p>
   * This method is intended for internal use only.
   */
  def checkUniqueOutputDestination(uri: DocumentURI): Boolean = {
    if (uri == null) {
      return true
    }
    if (allOutputDestinations == null) {
      allOutputDestinations = new HashSet[DocumentURI](20)
    }
    !(allOutputDestinations.contains(uri))
  }

  /**
   * Add a URI to the set of output destinations that cannot be written to, either because
   * they have already been written to, or because they have been read
   * @param uri A URI that is not available as an output destination
   */
  def addUnavailableOutputDestination(uri: DocumentURI): Unit = {
    if (allOutputDestinations == null) {
      allOutputDestinations = new HashSet[DocumentURI](20)
    }
    allOutputDestinations.add(uri)
  }

  def addToResultDocumentPool(uri: DocumentURI, doc: Node): Unit = {
    addUnavailableOutputDestination(uri)
    if (resultDocumentPool == null) {
      resultDocumentPool = new HashMap[DocumentURI, Node](20)
    }
    resultDocumentPool.put(uri, doc)
  }

  def getResultDocumentCount(): Int = {
    if ((resultDocumentPool == null)) 0 else resultDocumentPool.size
  }

  def importResults(ctrl: Controller): Unit = {
    this.resultDocumentPool = ctrl.resultDocumentPool
    this.principalOutputNode = ctrl.principalOutputNode
  }

  def getResultDocument(uri: String): Node = {
    if (uri == null || uri.length == 0) {
      return principalOutputNode
    }
    val docURI = new DocumentURI(uri)
    if (resultDocumentPool == null) {
      null
    } else if (resultDocumentPool.containsKey(docURI)) {
      resultDocumentPool.get(docURI)
    } else {
      null
    }
  }

  private def getJsResultURIset(): JavaScriptObject = {
    val uriArray = IXSLFunction.jsArray(resultDocumentPool.size)
    var poolSize = resultDocumentPool.size
    var uris = Array.ofDim[DocumentURI](poolSize)
    uris = resultDocumentPool.keySet.toArray(uris)
    poolSize -= 1
    var i = 0
    while (i <= poolSize) {
      IXSLFunction.jsSetArrayItem(uriArray, poolSize - i, uris(i).toString)
      i += 1
    }
    uriArray
  }

  /**
   * For JavaScriptAPI
   * @return a JavaScript object with a property for each result document.
   * The property name is the URI and the property value the document object
   */
  def getResultDocURIArray(): JavaScriptObject = {
    if (resultDocumentPool == null) {
      IXSLFunction.jsArray(0)
    } else {
      getJsResultURIset
    }
  }

  /**
   * Check whether an XSLT implicit result tree can be written. This is allowed only if no xsl:result-document
   * has been written for the principal output URI
   */
  def checkImplicitResultTree(): Unit = {
    if (principalResultURI != null && 
      !checkUniqueOutputDestination(new DocumentURI(principalResultURI))) {
      val err = new XPathException("Cannot write an implicit result document if an explicit result document has been written to the same URI: " + 
        principalResultURI)
      err.setErrorCode("XTDE1490")
      throw err
    }
  }

  /**
   * Test whether an explicit result tree has been written using xsl:result-document
   * @return true if the transformation has evaluated an xsl:result-document instruction
   */
  def hasThereBeenAnExplicitResultDocument(): Boolean = {
    (resultDocumentPool != null && resultDocumentPool.size > 0)
  }

  /**
   * Allocate a SequenceOutputter for a new output destination. Reuse the existing one
   * if it is available for reuse (this is designed to ensure that the TinyTree structure
   * is also reused, creating a forest of trees all sharing the same data structure)
   * @param size the estimated size of the output sequence
   * @return SequenceOutputter the allocated SequenceOutputter
   */
  def allocateSequenceOutputter(size: Int): SequenceOutputter = {
    if (reusableSequenceOutputter != null) {
      val out = reusableSequenceOutputter
      out.setSystemId(null)
      reusableSequenceOutputter = null
      out
    } else {
      new SequenceOutputter(this, size)
    }
  }

  /**
   * Accept a SequenceOutputter that is now available for reuse
   * @param out the SequenceOutputter that is available for reuse
   */
  def reuseSequenceOutputter(out: SequenceOutputter): Unit = {
    reusableSequenceOutputter = out
  }

  /**
   * Set the initial named template to be used as the entry point.
   * <p>
   * XSLT 2.0 allows a transformation to start by executing a named template, rather than
   * by matching an initial context node in a source document. This method may eventually
   * be superseded by a standard JAXP method once JAXP supports XSLT 2.0.
   * <p>
   * Note that any parameters supplied using [[#setParameter]] are used as the values
   * of global stylesheet parameters. There is no way to supply values for local parameters
   * of the initial template.
   *
   * @param expandedName The expanded name of the template in {uri}local format, or null
   * or a zero-length string to indicate that there should be no initial template.
   * @throws XPathException if there is no named template with this name
   * @since 8.4
   */
  def setInitialTemplate(expandedName: String): Unit = {
    if (expandedName == null || expandedName.length == 0) {
      initialTemplate = null
      initialTemplateName = null
      return
    }
    initialTemplateName = expandedName
    if (isInert) {
      return
    }
    val qName = StructuredQName.fromClarkName(expandedName)
    val t = getExecutable.getNamedTemplate(qName)
    if (t == null) {
      val err = new XPathException("The requested initial template, with expanded name " + 
        expandedName + 
        ", does not exist", "XTDE0040")
      reportFatalError(err)
      throw err
    } else if (t.hasRequiredParams()) {
      val err = new XPathException("The named template " + expandedName + 
        " has required parameters, so cannot be used as the entry point", "XTDE0060")
      reportFatalError(err)
      throw err
    } else {
      initialTemplate = t
    }
  }

  /**
   * Get the initial template
   * @return the name of the initial template, as an expanded name in Clark format if set, or null otherwise
   * @since 8.7
   */
  def getInitialTemplate(): String = {
    if (initialTemplate == null) {
      null
    } else {
      initialTemplate.getTemplateName.getClarkName
    }
  }

  /**
   * Make a PipelineConfiguration based on the properties of this Controller.
   * <p>
   * This interface is intended primarily for internal use, although it may be necessary
   * for applications to call it directly if they construct pull or push pipelines
   * @return a newly constructed PipelineConfiguration holding a reference to this
   * Controller as well as other configuration information.
   */
  def makePipelineConfiguration(): PipelineConfiguration = {
    val pipe = new PipelineConfiguration()
    pipe.setConfiguration(getConfiguration)
    pipe.setErrorListener(getErrorListener)
    pipe.setController(this)
    pipe
  }

  /**
   * Report a fatal error
   * @param err the error to be reported
   */
  def reportFatalError(err: XPathException): Unit = {
    if (!err.hasBeenReported()) {
      getErrorListener.error(err)
      err.setHasBeenReported(true)
    }
  }

  /**
   * Get the document pool. This is used only for source documents, not for stylesheet modules.
   * <p>
   * This method is intended for internal use only.
   *
   * @return the source document pool
   */
  def getDocumentPool(): DocumentPool = getConfiguration.getDocumentPool

  /**
   * Set the initial context item, when running XSLT invoked with a named template.
   * <p/>
   * When a transformation is invoked using the [[#transform]] method, the
   * initial context node is set automatically. This method is useful in XQuery,
   * to define an initial context node for evaluating global variables, and also
   * in XSLT 2.0, when the transformation is started by invoking a named template.
   *
   * <p>When an initial context item is set, it also becomes the context item used for
   * evaluating global variables. The two context items can only be different when the
   * [[#transform]] method is used to transform a document starting at a node other
   * than the root.</p>
   *
   * <p>In XQuery, the two context items are always
   * the same; in XSLT, the context node for evaluating global variables is the root of the
   * tree containing the initial context item.</p>
   *
   * @param item The initial context item.
   * @since 8.7
   */
  def setInitialContextItem(item: Item): Unit = {
    initialContextItem = item
    contextForGlobalVariables = item
  }

  /**
   * Make a builder for the selected tree model.
   *
   * @return an instance of the Builder for the chosen tree model
   * @since 8.4
   */
  def makeBuilder(): Builder = new LinkedTreeBuilder()

  /**
   * Say whether the transformation should perform whitespace stripping as defined
   * by the xsl:strip-space and xsl:preserve-space declarations in the stylesheet
   * in the case where a source tree is supplied to the transformation as a tree
   * (typically a DOMSource, or a Saxon NodeInfo).
   * The default is true. It is legitimate to suppress whitespace
   * stripping if the client knows that all unnecessary whitespace has already been removed
   * from the tree before it is processed. Note that this option applies to all source
   * documents for which whitespace-stripping is normally applied, that is, both the
   * principal source documents, and documents read using the doc(), document(), and
   * collection() functions. It does not apply to source documents that are supplied
   * in the form of a SAXSource or StreamSource, for which whitespace is stripped
   * during the process of tree construction.
   * <p>Generally, stripping whitespace speeds up the transformation if it is done
   * while building the source tree, but slows it down if it is applied to a tree that
   * has already been built. So if the same source tree is used as input to a number
   * of transformations, it is better to strip the whitespace once at the time of
   * tree construction, rather than doing it on-the-fly during each transformation.</p>
   * @param strip true if whitespace is to be stripped from supplied source trees
   * as defined by xsl:strip-space; false to suppress whitespace stripping
   * @since 9.3
   */
  def setStripSourceTrees(strip: Boolean): Unit = {
    stripSourceTrees = strip
  }

  /**
   * Ask whether the transformation will perform whitespace stripping for supplied source trees as defined
   * by the xsl:strip-space and xsl:preserve-space declarations in the stylesheet.
   * @return true unless whitespace stripping has been suppressed using
   * [[#setStripSourceTrees(boolean)]].
   * @since 9.3
   */
  def isStripSourceTree(): Boolean = stripSourceTrees

  /**
   * Add a document to the document pool, and check that it is suitable for use in this query or
   * transformation. This check rejects the document if document has been validated (and thus carries
   * type annotations) but the query or transformation is not schema-aware.
   * <p>
   * This method is intended for internal use only.
   *
   * @param doc the root node of the document to be added. Must not be null.
   * @param uri the document-URI property of this document. If non-null, the document is registered
   * in the document pool with this as its document URI.
   */
  def registerDocument(doc: DocumentInfo, uri: DocumentURI): Unit = {
    if (doc == null) {
      throw new NullPointerException("null")
    }
    if (uri != null) {
      getConfiguration.getDocumentPool.add(doc, uri)
    }
  }

  /**
   * Associate this Controller with a compiled stylesheet.
   * <p>
   * This method is intended for internal use only.
   *
   * @param sheet the compiled stylesheet
   */
  def setPreparedStylesheet(sheet: Executable): Unit = {
    executable = sheet
  }

  def getPreparedStylesheet(): Executable = executable

  /**
   * Initialize the controller ready for a new transformation. This method should not normally be called by
   * users (it is done automatically when transform() is invoked). However, it is available as a low-level API
   * especially for use with XQuery.
   */
  private def initializeController(): Unit = {
    if (executable != null) {
      setRuleManager(executable.getRuleManager)
    }
    bindery = new Bindery()
    executable.initializeBindery(bindery)
    defineGlobalParameters()
  }

  /**
   * Register the global parameters of the transformation or query. This should be called after a sequence
   * of calls on [[#setParameter]]. It checks that all required parameters have been supplied, and places
   * the values of the parameters in the Bindery to make them available for use during the query or
   * transformation.
   * <p>
   * This method is intended for internal use only
   */
  def defineGlobalParameters(): Unit = {
    executable.checkAllRequiredParamsArePresent(parameters)
    bindery.defineGlobalParameters(parameters)
  }

  /**
   * Get user data associated with a key. To retrieve user data, two objects are required:
   * an arbitrary object that may be regarded as the container of the data (originally, and
   * typically still, a node in a tree), and a name. The name serves to distingush data objects
   * associated with the same node by different client applications.
   * <p>
   * This method is intended primarily for internal use, though it may also be
   * used by advanced applications.
   *
   * @param key an object acting as a key for this user data value. This must be equal
   * (in the sense of the equals() method) to the key supplied when the data value was
   * registered using [[#setUserData]].
   * @param name the name of the required property
   * @return the value of the required property
   */
  def getUserData(key: AnyRef, name: String): AnyRef = {
    val keyValue = key.hashCode + " " + name
    userDataTable.get(keyValue)
  }

  /**
   * Set user data associated with a key. To store user data, two objects are required:
   * an arbitrary object that may be regarded as the container of the data (originally, and
   * typically still, a node in a tree), and a name. The name serves to distingush data objects
   * associated with the same node by different client applications.
   * <p>
   * This method is intended primarily for internal use, though it may also be
   * used by advanced applications.
   *
   * @param key an object acting as a key for this user data value. This can be any object, for example
   * a node or a string. If data for the given object and name already exists, it is overwritten.
   * @param name the name of the required property
   * @param data the value of the required property. If null is supplied, any existing entry
   * for the key is removed.
   */
  def setUserData(key: AnyRef, name: String, data: AnyRef): Unit = {
    val keyVal = key.hashCode + " " + name
    if (data == null) {
      userDataTable.remove(keyVal)
    } else {
      userDataTable.put(keyVal, data)
    }
  }

  /**
   * Perform a transformation from a Source document to a Result document.
   *
   * @exception XPathException if the transformation fails. As a
   *     special case, the method throws a TerminationException (a subclass
   *     of XPathException) if the transformation was terminated using
   *      xsl:message terminate="yes".
   * @param source The input for the source tree. May be null if and only if an
   * initial template has been supplied.
   * @return The root of the result tree.
   */
  def transform(source: NodeInfo, target: com.google.gwt.dom.client.Node): Node = {
    if (inUse) {
      throw new IllegalStateException("The Transformer is being used recursively or concurrently. This is not permitted.")
    }
    clearPerTransformationData()
    if (executable == null) {
      throw new XPathException("Stylesheet has not been prepared")
    }
    if (!dateTimePreset) {
      currentDateTime = null
    }
    getCurrentDateTime
    if (LogConfiguration.loggingIsEnabled()) {
      LogController.openTraceListener()
    }
    var success = false
    try {
      if (source == null) {
        if (initialTemplate == null) {
          throw new XPathException("Either a source document or an initial template must be specified")
        }
      } else {
        val mode = executable.getRuleManager.getMode(initialMode, false)
        if (mode == null || (initialMode != null && mode.isEmpty)) {
          throw new XPathException("Requested initial mode " + 
            (if (initialMode == null) "" else initialMode.getDisplayName) + 
            " does not exist", "XTDE0045")
        }
        if (source.getSystemId != null) {
          registerDocument(source.getDocumentRoot, new DocumentURI(source.getSystemId))
        }
      }
      if (executable == null) {
        throw new XPathException("Stylesheet has not been compiled")
      }
      val initialContext = newXPathContext()
      if (source != null) {
        initialContextItem = source
        contextForGlobalVariables = source.getRoot
        val currentIter = SingletonIterator.makeIterator(source)
        if (initialTemplate != null) {
          initialContext.setSingletonFocus(initialContextItem)
        } else {
          initialContext.setCurrentIterator(currentIter)
        }
      }
      initializeController()
      val pipe = makePipelineConfiguration()
      val result = openResult(pipe, initialContext, target, ResultDocument.APPEND_CONTENT)
      if (initialTemplate == null) {
        initialContextItem = source
        val mode = getRuleManager.getMode(initialMode, false)
        if (mode == null || (initialMode != null && mode.isEmpty)) {
          throw new XPathException("Requested initial mode " + 
            (if (initialMode == null) "" else initialMode.getDisplayName) + 
            " does not exist", "XTDE0045")
        }
        var tc = ApplyTemplates.applyTemplates(initialContext.getCurrentIterator, mode, null, null, initialContext, 
          null)
        while (tc != null) {
          tc = tc.processLeavingTail()
        }
      } else {
        val t = initialTemplate
        val c2 = initialContext.newContext()
        c2.setParameters(t.getNumberOfSlots, new ParameterSet(), new ParameterSet())
        var tc = t.expand(c2)
        while (tc != null) {
          tc = tc.processLeavingTail()
        }
      }
      closeMessageEmitter()
      closeResult(result, initialContext)
      pendingUpdateList.apply(initialContext)
      success = true
      principalOutputNode = openHTMLWriter.getNode
      principalOutputNode
    } finally {
      inUse = false
      principalResultURI = null
      if (LogConfiguration.loggingIsEnabled()) {
        LogController.closeTraceListener(success)
      }
    }
  }

  private def closeMessageEmitter(): Unit = {
  }

  def closeResult(result: Receiver, initialContext: XPathContext): Unit = {
    val out = initialContext.getReceiver
    out.endDocument()
    out.close()
  }

  private def checkPrincipalURI(result: Receiver, initialContext: XPathContext): Unit = {
    val out = initialContext.getReceiver
    if (out.isInstanceOf[ComplexContentOutputter] && 
      out.asInstanceOf[ComplexContentOutputter].contentHasBeenWritten()) {
      if (principalResultURI != null) {
        val documentKey = new DocumentURI(principalResultURI)
        if (!checkUniqueOutputDestination(documentKey)) {
          val err = new XPathException("Cannot write more than one result document to the same URI, or write to a URI that has been read: " + 
            documentKey)
          err.setErrorCode("XTDE1490")
          throw err
        } else {
          addUnavailableOutputDestination(documentKey)
        }
      }
    }
  }

  def openResult(pipe: PipelineConfiguration, 
      initialContext: XPathContext, 
      root: Node, 
      method: Int): Receiver = {
    val writer = new HTMLWriter()
    writer.setPipelineConfiguration(pipe)
    val reducer = new NamespaceReducer()
    reducer.setUnderlyingReceiver(writer)
    reducer.setPipelineConfiguration(pipe)
    writer.setNode(root)
    var receiver = reducer
    var openNow = false
    if (getExecutable.createsSecondaryResult()) {
      receiver = new ImplicitResultChecker(receiver, this)
      receiver.setPipelineConfiguration(pipe)
    } else {
      openNow = true
    }
    initialContext.changeOutputDestination(receiver, true)
    if (openNow) {
      val out = initialContext.getReceiver
      out.open()
      out.startDocument()
    }
    openHTMLWriter = writer
    receiver
  }

  /**
   * Supply a parameter using Saxon-specific representations of the name and value
   * @param qName The structured representation of the parameter name
   * @param value The value of the parameter, or null to remove a previously set value
   */
  def setParameter(qName: StructuredQName, value: Sequence): Unit = {
    if (parameters == null) {
      parameters = new HashMap[StructuredQName, Sequence]()
    }
    parameters.put(qName, value)
  }

  def removeParameter(qName: StructuredQName): Unit = {
    if (parameters != null) {
      parameters.remove(qName)
    }
  }

  /**
   * Reset the parameters to a null list.
   */
  def clearParameters(): Unit = {
    parameters = null
  }

  /**
   * Get a parameter to the transformation. This returns the value of a parameter
   * that has been previously set using the [[#setParameter]] method. The value
   * is returned exactly as supplied, that is, before any conversion to an XPath value.
   *
   * @param qName the name of the required parameter
   * @return the value of the parameter, if it exists, or null otherwise
   */
  def getParameter(qName: StructuredQName): Sequence = {
    if (parameters == null) {
      return null
    }
    parameters.get(qName)
  }

  /**
   * Set the current date and time for this query or transformation.
   * This method is provided primarily for testing purposes, to allow tests to be run with
   * a fixed date and time. The supplied date/time must include a timezone, which is used
   * as the implicit timezone.
   *
   * <p>Note that comparisons of date/time values currently use the implicit timezone
   * taken from the system clock, not from the value supplied here.</p>
   *
   * @param dateTime the date/time value to be used as the current date and time
   * @throws IllegalStateException if a current date/time has already been
   * established by calling getCurrentDateTime(), or by a previous call on setCurrentDateTime()
   */
  def setCurrentDateTime(dateTime: DateTimeValue): Unit = {
    if (currentDateTime == null) {
      if (dateTime.getComponent(Component.TIMEZONE) == null) {
        throw new XPathException("No timezone is present in supplied value of current date/time")
      }
      currentDateTime = dateTime.adjustTimezone(getConfiguration.getImplicitTimezone).asInstanceOf[DateTimeValue]
      dateTimePreset = true
    } else {
      throw new IllegalStateException("Current date and time can only be set once, and cannot subsequently be changed")
    }
  }

  /**
   * Make an XPathContext object for expression evaluation.
   * <p>
   * This method is intended for internal use.
   *
   * @return the new XPathContext
   */
  def newXPathContext(): XPathContext = new XPathContext(this)

  /**
   * Set the last remembered node, for node numbering purposes.
   * <p>
   * This method is strictly for internal use only.
   *
   * @param node the node in question
   * @param number the number of this node
   */
  def setRememberedNumber(node: NodeInfo, number: Int): Unit = {
    lastRememberedNode = node
    lastRememberedNumber = number
  }

  /**
   * Get the number of a node if it is the last remembered one.
   * <p>
   * This method is strictly for internal use only.
   *
   * @param node the node for which remembered information is required
   * @return the number of this node if known, else -1.
   */
  def getRememberedNumber(node: NodeInfo): Int = {
    if (lastRememberedNode == node) {
      return lastRememberedNumber
    }
    -1
  }

  /**
   * Set a TraceListener, replacing any existing TraceListener
   * <p>This method has no effect unless the stylesheet or query was compiled
   * with tracing enabled.</p>
   *
   * @param listener the TraceListener to be set. May be null, in which case
   *                 trace events will not be reported
   * @since 9.2
   */
  @BeanProperty
  var traceListener: TraceListener = _

  /**
   * Test whether instruction execution is being traced. This will be true
   * if (a) at least one TraceListener has been registered using the
   * [[#addTraceListener]] method, and (b) tracing has not been temporarily
   * paused using the [[#pauseTracing]] method.
   *
   * @return true if tracing is active, false otherwise
   * @since 8.4
   */
  def isTracing(): Boolean = traceListener != null && !tracingPaused

  /**
   * Pause or resume tracing. While tracing is paused, trace events are not sent to any
   * of the registered TraceListeners.
   *
   * @param pause true if tracing is to pause; false if it is to resume
   * @since 8.4
   */
  private var tracingPaused: Boolean = false

  def pauseTracing(pause: Boolean): Unit = {
    tracingPaused = pause
  }

  /**
   * Adds the specified trace listener to receive trace events from
   * this instance. Note that although TraceListeners can be added
   * or removed dynamically, this has no effect unless the stylesheet
   * or query has been compiled with tracing enabled.
   * Conversely, if this property has been set in the
   * Configuration or TransformerFactory, the TraceListener will automatically
   * be added to every Controller that uses that Configuration.
   *
   * @param trace the trace listener. If null is supplied, the call has no effect.
   * @since 8.4
   */
  def addTraceListener(trace: TraceListener): Unit = {
    if (trace != null) {
    }
  }

  /**
   * Removes the specified trace listener so that the listener will no longer
   * receive trace events.
   *
   * @param trace the trace listener.
   * @since 8.4
   */
  def removeTraceListener(trace: TraceListener): Unit = {
  }

  def setOutputProperties(props: AnyRef): Unit = {
  }
}

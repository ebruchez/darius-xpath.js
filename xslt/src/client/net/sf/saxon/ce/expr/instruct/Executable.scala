// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import java.util.{HashMap, HashSet}

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.functions.FunctionLibraryList
import client.net.sf.saxon.ce.om.{CopyOptions, DocumentInfo, Sequence, StructuredQName}
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans._
import client.net.sf.saxon.ce.tree.linked.{DocumentImpl, LinkedTreeBuilder}
import client.net.sf.saxon.ce.value.DecimalValue

import scala.beans.BeanProperty


import scala.collection.JavaConversions._

/**
 * A compiled stylesheet or a query in executable form.
 * Note that the original stylesheet tree is not retained.
 */
class Executable(config: Configuration) {

  @transient private var config: Configuration = _

  @BeanProperty
  var stripperRules: StripSpaceRules = new StripSpaceRules()

  @BeanProperty
  lazy val keyManager = new KeyManager()

  private var numberOfGlobals: Int = _

  @BeanProperty
  var functionLibrary: FunctionLibraryList = _

  private var requiredParams: HashSet[StructuredQName] = null

  var createsSecondaryResult: Boolean = false

  @BeanProperty
  var errorCount: Int = 0

  @BeanProperty
  lazy var decimalFormatManager: DecimalFormatManager = new DecimalFormatManager()

  @BeanProperty
  var ruleManager: RuleManager = new RuleManager()

  private var namedTemplateTable: HashMap[StructuredQName, Template] = _

  setConfiguration(config)

  /**
   * Get the configuration
   * @return the Configuration
   */
  def getConfiguration(): Configuration = config

  /**
   * Allocate a slot number for a global variable
   * @return the allocated slot number
   */
  def allocateGlobalVariableSlot(): Int = numberOfGlobals += 1

  /**
   * Allocate space in bindery for all the variables needed
   * @param bindery The bindery to be initialized
   */
  def initializeBindery(bindery: Bindery): Unit = {
    bindery.allocateGlobals(numberOfGlobals)
  }

  /**
   * Add a required parameter. Used in XSLT only.
   * @param qName the name of the required parameter
   */
  def addRequiredParam(qName: StructuredQName): Unit = {
    if (requiredParams == null) {
      requiredParams = new HashSet[StructuredQName](5)
    }
    requiredParams.add(qName)
  }

  /**
   * Check that all required parameters have been supplied. Used in XSLT only.
   * @param params the set of parameters that have been supplied
   * @throws XPathException if there is a required parameter for which no value has been supplied
   */
  def checkAllRequiredParamsArePresent(params: HashMap[StructuredQName, Sequence]): Unit = {
    if (requiredParams == null) {
      return
    }
    for (req <- requiredParams if params == null || !params.containsKey(req)) {
      throw new XPathException("No value supplied for required parameter " + req.getDisplayName, "XTDE0050")
    }
  }

  /**
   * Set whether this executable represents a stylesheet that uses xsl:result-document
   * to create secondary output documents
   * @param flag true if the executable uses xsl:result-document
   */
  def setCreatesSecondaryResult(flag: Boolean): Unit = {
    createsSecondaryResult = flag
  }

  /**
   * Make a Transformer from this Templates object.
   * @return the new Transformer (always a Controller)
   * @see client.net.sf.saxon.ce.Controller
   */
  def newTransformer(): Controller = {
    val c = new Controller(getConfiguration, this)
    c.setPreparedStylesheet(this)
    c
  }

  /**
   * Set the configuration in which this stylesheet is compiled.
   * Intended for internal use.
   * @param config the configuration to be used.
   */
  def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }

  /**
   * Prepare a stylesheet from a Source document
   * @param doc the source document containing the stylesheet
   * @throws client.net.sf.saxon.ce.trans.XPathException if compilation of the stylesheet fails for any reason
   */
  def prepare(doc: DocumentInfo): Unit = {
    var message = ""
    try {
      setStylesheetDocument(loadStylesheetModule(doc))
    } catch {
      case e: XPathException => {
        if (errorCount == 0) {
          errorCount += 1
        }
        message = e.getMessage + ". "
      }
    }
    if (errorCount > 0) {
      throw new XPathException("Failed to compile stylesheet. " + message + errorCount + 
        (if (errorCount == 1) " error " else " errors ") + 
        "detected.")
    }
  }

  /**
   * Build the tree representation of a stylesheet module
   * @param rawDoc the stylesheet module, typically as a DOM, before stripping of
   * whitespace, comments, and PIs
   * @return the root Document node of the tree containing the stylesheet
   *         module
   * @throws client.net.sf.saxon.ce.trans.XPathException if XML parsing or tree
   *                        construction fails
   */
  def loadStylesheetModule(rawDoc: DocumentInfo): DocumentImpl = {
    val nodeFactory = new StyleNodeFactory(config)
    val styleBuilder = new LinkedTreeBuilder()
    val pipe = getConfiguration.makePipelineConfiguration()
    styleBuilder.setPipelineConfiguration(pipe)
    styleBuilder.setSystemId(rawDoc.getSystemId)
    styleBuilder.setNodeFactory(nodeFactory)
    val startTagBuffer = new StartTagBuffer()
    val nsReducer = new NamespaceReducer()
    nsReducer.setUnderlyingReceiver(startTagBuffer)
    val useWhenFilter = new UseWhenFilter(startTagBuffer, nsReducer)
    useWhenFilter.setUnderlyingReceiver(styleBuilder)
    startTagBuffer.setUnderlyingReceiver(useWhenFilter)
    val styleStripper = new StylesheetStripper()
    styleStripper.setUnderlyingReceiver(nsReducer)
    val commentStripper = new CommentStripper()
    commentStripper.setUnderlyingReceiver(styleStripper)
    commentStripper.setPipelineConfiguration(pipe)
    commentStripper.open()
    rawDoc.copy(commentStripper, CopyOptions.ALL_NAMESPACES)
    commentStripper.close()
    val doc = styleBuilder.getCurrentRoot.asInstanceOf[DocumentImpl]
    styleBuilder.reset()
    doc
  }

  /**
   * Create a PreparedStylesheet from a supplied DocumentInfo
   * Note: the document must have been built using the StyleNodeFactory
   * @param doc the document containing the stylesheet module
   * @throws client.net.sf.saxon.ce.trans.XPathException if the document supplied
   *                        is not a stylesheet
   */
  protected def setStylesheetDocument(doc: DocumentImpl): Unit = {
    var styleDoc = doc
    val topnode = styleDoc.getDocumentElement.asInstanceOf[StyleElement]
    if (topnode == null) {
      throw new XPathException("Failed to parse stylesheet")
    }
    if (topnode.isInstanceOf[LiteralResultElement]) {
      styleDoc = topnode.asInstanceOf[LiteralResultElement].makeStylesheet(this)
    }
    if (!(styleDoc.getDocumentElement.isInstanceOf[XSLStylesheet])) {
      throw new XPathException("Outermost element of stylesheet is not xsl:stylesheet or xsl:transform or literal result element")
    }
    val top = styleDoc.getDocumentElement.asInstanceOf[XSLStylesheet]
    if (top.getEffectiveVersion.compareTo(DecimalValue.TWO) != 
      0) {
      getConfiguration.issueWarning("Running an XSLT " + top.getEffectiveVersion + " stylesheet with an XSLT 2.0 processor")
    }
    val psm = new PrincipalStylesheetModule(top, 0)
    psm.setExecutable(this)
    psm.setVersion(top.getAttributeValue("", "version"))
    psm.createFunctionLibrary()
    setFunctionLibrary(psm.getFunctionLibrary)
    top.setPrincipalStylesheetModule(psm)
    psm.preprocess()
    psm.compileStylesheet()
  }

  /**
   * Get the named template with a given name.
   *
   * @param qName The template name
   * @return The template (of highest import precedence) with this name if there is one;
   *         null if none is found.
   */
  def getNamedTemplate(qName: StructuredQName): Template = {
    if (namedTemplateTable == null) {
      return null
    }
    namedTemplateTable.get(qName)
  }

  /**
   * Register the named template with a given name
   * @param templateName the name of a named XSLT template
   * @param template the template
   */
  def putNamedTemplate(templateName: StructuredQName, template: Template): Unit = {
    if (namedTemplateTable == null) {
      namedTemplateTable = new HashMap(32)
    }
    namedTemplateTable.put(templateName, template)
  }

  /**
   * Report a compile time error. This calls the errorListener to output details
   * of the error, and increments an error count.
   * @param err the exception containing details of the error
   * @throws client.net.sf.saxon.ce.trans.XPathException if the ErrorListener decides that the
   *                              error should be reported
   */
  def reportError(err: XPathException): Unit = {
    if (!err.hasBeenReported()) {
      errorCount += 1
      config.getErrorListener.error(err)
      err.setHasBeenReported(true)
    }
  }
}

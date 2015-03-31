// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.expr.parser.CodeInjector
import client.net.sf.saxon.ce.expr.sort.SortKeyDefinition
import client.net.sf.saxon.ce.functions.Current
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.pattern._
import client.net.sf.saxon.ce.trace.Location
import client.net.sf.saxon.ce.trace.XSLTTraceListener
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.linked.ElementImpl
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.tree.util.NamespaceIterator
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.tree.util.SourceLocator
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.value.DecimalValue
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.logging.client.LogConfiguration
import java.util._
import StyleElement._
//remove if not needed
import scala.collection.JavaConversions._

object StyleElement {

  val REPORT_ALWAYS = 1

  val REPORT_UNLESS_FORWARDS_COMPATIBLE = 2

  val REPORT_IF_INSTANTIATED = 3

  val REPORT_UNLESS_FALLBACK_AVAILABLE = 4

  val ACTION_VALIDATE = 1

  val ACTION_COMPILE = 2

  val ACTION_TYPECHECK = 4

  val ACTION_OPTIMIZE = 8

  val ACTION_FIXUP = 16

  val ACTION_PROCESS_ATTRIBUTES = 32

  /**
   * Create a trace instruction to wrap a real instruction
   *
   * @param source the parent element
   * @param child  the compiled expression tree for the instruction to be traced
   * @return a wrapper instruction that performs the tracing (if activated at run-time)
   */
  def makeTraceInstruction(source: StyleElement, child: Expression): Expression = {
    if (child.isInstanceOf[TraceExpression] && !source.isInstanceOf[StylesheetProcedure]) {
      return child
    }
    val injector = LogController.getTraceListener.asInstanceOf[XSLTTraceListener]
      .getCodeInjector
    var construct = source.getNodeName
    var qName: StructuredQName = null
    if (source.isInstanceOf[LiteralResultElement]) {
      construct = Location.LITERAL_RESULT_ELEMENT
      qName = source.getNodeName
    } else {
      qName = source.getObjectName
    }
    val tracer = injector.inject(child, source.getStaticContext, construct, qName)
    tracer.setSourceLocator(source)
    tracer
  }
}

/**
 * Abstract superclass for all element nodes in the stylesheet.
 * <p>Note: this class implements Locator. The element retains information about its own location
 * in the stylesheet, which is useful when an XSLT static error is found.</p>
 */
abstract class StyleElement extends ElementImpl with Container with SourceLocator {

  protected var extensionNamespaces: Array[String] = null

  private var excludedNamespaces: Array[String] = null

  protected var version: DecimalValue = null

  protected var staticContext: StaticContext = null

  protected var validationError: XPathException = null

  protected var reportingCircumstances: Int = REPORT_ALWAYS

  protected var defaultXPathNamespace: String = null

  protected var defaultCollationName: String = null

  private var objectName: StructuredQName = _

  private var containingStylesheet: XSLStylesheet = _

  protected var actionsCompleted: Int = 0

  def getExecutable(): Executable = getPreparedStylesheet

  def getConfiguration(): Configuration = getPreparedStylesheet.getConfiguration

  /**
   * Get the LocationProvider allowing location identifiers to be resolved.
   */
  def getSourceLocator(): SourceLocator = this

  def getLocation(): String = {
    Navigator.getPath(this) + " in " + getBaseURI
  }

  /**
   * Get the static context for expressions on this element
   * @return the static context
   */
  def getStaticContext(): StaticContext = {
    if (staticContext == null) {
      staticContext = new ExpressionContext(this)
    }
    staticContext
  }

  /**
   * Get the granularity of the container.
   * @return 0 for a temporary container created during parsing; 1 for a container
   *         that operates at the level of an XPath expression; 2 for a container at the level
   *         of a global function or template
   */
  def getContainerGranularity(): Int = 1

  /**
   * Make an expression visitor
   * @return the expression visitor
   */
  def makeExpressionVisitor(): ExpressionVisitor = {
    ExpressionVisitor.make(getStaticContext, getExecutable)
  }

  /**
   * Make this node a substitute for a temporary one previously added to the tree. See
   * StyleNodeFactory for details. "A node like the other one in all things but its class".
   * Note that at this stage, the node will not yet be known to its parent, though it will
   * contain a reference to its parent; and it will have no children.
   * @param temp the element which this one is substituting for
   */
  def substituteFor(temp: StyleElement): Unit = {
    setRawParent(temp.getRawParent)
    setAttributeList(temp.getAttributeList)
    setNamespaceList(temp.getNamespaceList)
    setNodeName(temp.getNodeName)
    setRawSequenceNumber(temp.getRawSequenceNumber)
    extensionNamespaces = temp.extensionNamespaces
    excludedNamespaces = temp.excludedNamespaces
    version = temp.version
    validationError = temp.validationError
    reportingCircumstances = temp.reportingCircumstances
  }

  /**
   * Set a validation error. This is an error detected during construction of this element on the
   * stylesheet, but which is not to be reported until later.
   * @param reason        the details of the error
   * @param circumstances a code identifying the circumstances under which the error is to be reported
   */
  def setValidationError(reason: XPathException, circumstances: Int): Unit = {
    validationError = reason
    reportingCircumstances = circumstances
  }

  /**
   * Ask whether this node is an instruction. The default implementation says it isn't.
   * @return true if this element is an instruction
   */
  def isInstruction(): Boolean = false

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import). The default implementation returns false
   * @return true if the element is a permitted child of xsl:stylesheet or xsl:transform
   */
  def isDeclaration(): Boolean = false

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction). Default implementation returns Type.ITEM, indicating
   * that we don't know, it might be anything. Returns null in the case of an element
   * such as xsl:sort or xsl:variable that can appear in a sequence constructor but
   * contributes nothing to the result sequence.
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = AnyItemType.getInstance

  /**
   * Get the most general type of item returned by the children of this instruction
   * @return the lowest common supertype of the item types returned by the children
   */
  protected def getCommonChildItemType(): ItemType = {
    var t = EmptySequenceTest.getInstance
    for (child ← allChildren()) {
      if (child.isInstanceOf[StyleElement]) {
        val ret = child.asInstanceOf[StyleElement].getReturnedItemType
        if (ret != null) {
          t = Type.getCommonSuperType(t, ret)
        }
      } else {
        t = Type.getCommonSuperType(t, NodeKindTest.TEXT)
      }
      if (t == AnyItemType.getInstance) {
        return t
      }
    }
    t
  }

  /**
   * Mark tail-recursive calls on templates and functions.
   * For most instructions, this returns false.
   * @return true if one or more tail calls were identified
   */
  protected def markTailCalls(): Boolean = false

  /**
   * Determine whether this type of element is allowed to contain a sequence constructor
   * @return true if this instruction is allowed to contain a sequence constructor
   */
  protected def mayContainSequenceConstructor(): Boolean = false

  /**
   * Determine whether this type of element is allowed to contain an xsl:param element
   * @param attName if null, the method tests whether an xsl:param child is allowed.
   *                If non-null, it tests whether an xsl:param child with the given attribute name is allowed
   * @return true if this element is allowed to contain an xsl:param
   */
  protected def mayContainParam(attName: String): Boolean = false

  /**
   * Get the containing XSLStylesheet element
   * @return the XSLStylesheet element representing the outermost element of the containing
   *         stylesheet module. Exceptionally, return null if there is no containing XSLStylesheet element
   */
  def getContainingStylesheet(): XSLStylesheet = {
    if (containingStylesheet == null) {
      var node = this
      while (node != null && !node.isInstanceOf[XSLStylesheet]) {
        node = node.getParent
      }
      containingStylesheet = node.asInstanceOf[XSLStylesheet]
    }
    containingStylesheet
  }

  /**
   * Make a structured QName, using this Element as the context for namespace resolution, and
   * registering the code in the namepool. If the name is unprefixed, the
   * default namespace is <b>not</b> used.
   * @param lexicalQName The lexical QName as written, in the form "[prefix:]localname". The name must have
   *                     already been validated as a syntactically-correct QName. Leading and trailing whitespace
   *                     will be trimmed
   * @return the StructuredQName representation of this lexical QName
   * @throws XPathException     if the qname is not a lexically-valid QName, or if the name
   *                            is in a reserved namespace.
   * @throws NamespaceException if the prefix of the qname has not been declared
   */
  def makeQName(lexicalQName: String): StructuredQName = {
    var qName: StructuredQName = null
    try {
      qName = StructuredQName.fromLexicalQName(lexicalQName, "", new InscopeNamespaceResolver(this))
    } catch {
      case e: XPathException ⇒ {
        e.setIsStaticError(true)
        val code = e.getErrorCodeLocalPart
        if ("FONS0004" == code) {
          e.setErrorCode("XTSE0280")
        } else if ("FOCA0002" == code) {
          e.setErrorCode("XTSE0020")
        } else if (code == null) {
          e.setErrorCode("XTSE0020")
        }
        throw e
      }
    }
    if (NamespaceConstant.isReserved(qName.getNamespaceURI)) {
      val err = new XPathException("Namespace prefix " + qName.getPrefix + " refers to a reserved namespace")
      err.setIsStaticError(true)
      err.setErrorCode("XTSE0080")
      throw err
    }
    qName
  }

  /**
   * Process the attributes of this element and all its children
   * @throws XPathException in the event of a static error being detected
   */
  protected def processAllAttributes(): Unit = {
    if (!this.isInstanceOf[LiteralResultElement]) {
      processDefaultCollationAttribute("")
    }
    getStaticContext
    processAttributes()
    for (child ← allChildren() if child.isInstanceOf[StyleElement]) {
      child.asInstanceOf[StyleElement].processAllAttributes()
    }
  }

  /**
   * Process the standard attributes such as [xsl:]default-collation
   * @param namespace either "" to find the attributes in the null namespace,
   *                  or NamespaceConstant.XSLT to find them in the XSLT namespace
   */
  def processStandardAttributes(namespace: String): Unit = {
    processDefaultCollationAttribute(namespace)
    processExtensionElementAttribute(namespace)
    processExcludedNamespaces(namespace)
    processVersionAttribute(namespace)
    processDefaultXPathNamespaceAttribute(namespace)
  }

  /**
   * Process the attribute list for the element. This is a wrapper method that calls
   * prepareAttributes (provided in the subclass) and traps any exceptions
   */
  protected def processAttributes(): Unit = {
    try {
      prepareAttributes()
    } catch {
      case err: XPathException ⇒ compileError(err)
    }
  }

  private var permittedAttributes: Set[String] = new HashSet[String](8)

  protected def checkAttribute(name: String, flags: String): AnyRef = {
    permittedAttributes.add(name)
    val `val` = getAttributeList.getValue("", name)
    if (`val` == null) {
      if (flags.contains("1")) {
        reportAbsence(getDisplayName + "/" + name)
      }
    } else {
      for (i ← 0 until flags.length) flags.charAt(i) match {
        case 'a' ⇒ return makeAttributeValueTemplate(`val`)
        case 'b' ⇒
          var yesNo = Whitespace.trim(`val`)
          if ("yes" == yesNo) {
            return true
          } else if ("no" == yesNo) {
            return false
          } else {
            compileError("The @" + name + " attribute must have the value 'yes' or 'no'", "XTSE0020")
          }

        case 'e' ⇒ return makeExpression(`val`)
        case 'p' ⇒ return makePattern(`val`)
        case 'q' ⇒ try {
          return makeQName(`val`)
        } catch {
          case e: NamespaceException ⇒ compileError(e.getMessage, "XTSE0280")
        }
        case 's' ⇒ return `val`
        case 't' ⇒ compileError("The @type attribute is available only with a schema-aware XSLT processor",
          "XTSE1660")
        case 'v' ⇒
          if (`val` != "strip") {
            compileError("The @type attribute is available only with a schema-aware XSLT processor", 
              "XTSE1660")
          }
          return null

        case 'w' ⇒ return Whitespace.collapseWhitespace(`val`).toString
        case 'z' ⇒ return makeSequenceType(`val`)
      }
    }
    null
  }

  protected def checkForUnknownAttributes(): Unit = {
    val atts = getAttributeList
    for (a ← 0 until atts.getLength) {
      val qn = atts.getStructuredQName(a)
      if (qn.getNamespaceURI == "" && !permittedAttributes.contains(qn.getLocalName)) {
        checkUnknownAttribute(qn)
      }
    }
  }

  /**
   * Check whether an unknown attribute is permitted.
   * @param nc The name code of the attribute name
   * @throws XPathException (and reports the error) if this is an attribute
   *                        that is not permitted on the containing element
   */
  private def checkUnknownAttribute(nc: StructuredQName): Unit = {
    if (forwardsCompatibleModeIsEnabled()) {
      return
    }
    val attributeURI = nc.getNamespaceURI
    val elementURI = getURI
    val localName = nc.getLocalName
    if (localName == "default-collation" || localName == "xpath-default-namespace" ||
      localName == "extension-element-prefixes" ||
      localName == "exclude-result-prefixes" ||
      localName == "version" ||
      localName == "use-when") {
      if (elementURI == NamespaceConstant.XSLT) {
        if ("" == attributeURI) {
          return
        }
      } else if (attributeURI == NamespaceConstant.XSLT && isInstruction) {
        return
      }
    }
    if ("" == attributeURI || NamespaceConstant.XSLT == attributeURI) {
      compileError("Attribute " + Err.wrap(nc.getDisplayName, Err.ATTRIBUTE) + 
        " is not allowed on element " + 
        Err.wrap(getDisplayName, Err.ELEMENT), "XTSE0090")
    }
  }

  /**
   * Set the attribute list for the element. This is called to process the attributes (note
   * the distinction from processAttributes in the superclass).
   * Must be supplied in a subclass
   */
  protected def prepareAttributes(): Unit

  /**
   * Find the last child instruction of this instruction. Returns null if
   * there are no child instructions, or if the last child is a text node.
   * @return the last child instruction, or null if there are no child instructions
   */
  protected def getLastChildInstruction(): StyleElement = {
    var last: StyleElement = null
    for (child ← allChildren()) {
      last = if (child.isInstanceOf[StyleElement]) child.asInstanceOf[StyleElement] else null
    }
    last
  }

  /**
   * Compile an XPath expression in the context of this stylesheet element
   * @param expression the source text of the XPath expression
   * @return the compiled expression tree for the XPath expression
   */
  def makeExpression(expression: String): Expression = {
    try {
      ExpressionTool.make(expression, getStaticContext, this, 0, Token.EOF, this)
    } catch {
      case err: XPathException ⇒ {
        err.setLocator(this)
        compileError(err)
        val erexp = new ErrorExpression(err)
        erexp.setSourceLocator(this)
        erexp.setContainer(this)
        erexp
      }
    }
  }

  /**
   * Make a pattern in the context of this stylesheet element
   * @param pattern the source text of the pattern
   * @return the compiled pattern
   */
  def makePattern(pattern: String): Pattern = {
    try {
      Pattern.make(pattern, getStaticContext, this)
    } catch {
      case err: XPathException ⇒ {
        compileError(err)
        new NodeTestPattern(AnyNodeTest.getInstance)
      }
    }
  }

  /**
   * Make an attribute value template in the context of this stylesheet element
   * @param expression the source text of the attribute value template
   * @return a compiled XPath expression that computes the value of the attribute (including
   *         concatenating the results of embedded expressions with any surrounding fixed text)
   */
  protected def makeAttributeValueTemplate(expression: String): Expression = {
    try {
      AttributeValueTemplate.make(expression, this, getStaticContext)
    } catch {
      case err: XPathException ⇒ {
        compileError(err)
        new StringLiteral(expression)
      }
    }
  }

  /**
   * Process an attribute whose value is a SequenceType
   * @param sequenceType the source text of the attribute
   * @return the processed sequence type
   * @throws XPathException if the syntax is invalid or for example if it refers to a type
   *                        that is not in the static context
   */
  def makeSequenceType(sequenceType: String): SequenceType = {
    try {
      val parser = new ExpressionParser()
      parser.setLanguage(ExpressionParser.XPATH)
      parser.parseSequenceType(sequenceType, getStaticContext)
    } catch {
      case err: XPathException ⇒ {
        compileError(err)
        SequenceType.ANY_SEQUENCE
      }
    }
  }

  /**
   * Process the [xsl:]extension-element-prefixes attribute if there is one
   * @param ns the namespace URI of the attribute - either the XSLT namespace or "" for the null namespace
   * @throws XPathException in the event of a bad prefix
   */
  protected def processExtensionElementAttribute(ns: String): Unit = {
    val ext = getAttributeValue(ns, "extension-element-prefixes")
    if (ext != null) {
      extensionNamespaces = processPrefixList(ext, false)
    }
  }

  /**
   * Process the [xsl:]exclude-result-prefixes attribute if there is one
   * @param ns the namespace URI of the attribute required, either the XSLT namespace or ""
   * @throws XPathException in the event of a bad prefix
   */
  protected def processExcludedNamespaces(ns: String): Unit = {
    val ext = getAttributeValue(ns, "exclude-result-prefixes")
    if (ext != null) {
      excludedNamespaces = processPrefixList(ext, true)
    }
  }

  /**
   * Process a string containing a whitespace-separated sequence of namespace prefixes
   * @param in  the input string
   * @param allowAll true if the token #all is permitted
   * @return the list of corresponding namespace URIs
   * @throws XPathException if there is a bad prefix
   */
  private def processPrefixList(in: String, allowAll: Boolean): Array[String] = {
    val resolver = new InscopeNamespaceResolver(this)
    if (allowAll && "#all" == Whitespace.trim(in)) {
      val codes = NamespaceIterator.iterateNamespaces(this)
      val result = new ArrayList[String]()
      while (codes.hasNext) {
        result.add(codes.next().getURI)
      }
      result.toArray(Array.ofDim[String](result.size))
    } else {
      val tokens = Whitespace.tokenize(in)
      var count = tokens.size
      val result = Array.ofDim[String](count)
      count = 0
      for (s ← tokens) {
        if ("#default" == s) {
          s = ""
        } else if (allowAll && "#all" == s) {
          compileError("In exclude-result-prefixes, cannot mix #all with other values", "XTSE0020")
        }
        val uri = resolver.getURIForPrefix(s, true)
        if (uri == null) {
          compileError("Prefix " + s + " is undeclared", "XTSE1430")
        }
        result(count += 1) = uri
      }
      result
    }
  }

  /**
   * Process the [xsl:]version attribute if there is one
   * @param ns the namespace URI of the attribute required, either the XSLT namespace or ""
   * @throws XPathException if the value is invalid
   */
  protected def processVersionAttribute(ns: String): Unit = {
    val v = Whitespace.trim(getAttributeValue(ns, "version"))
    if (v != null) {
      val `val` = DecimalValue.makeDecimalValue(v)
      if (`val`.isInstanceOf[ValidationFailure]) {
        compileError("The version attribute must be a decimal literal", "XTSE0110")
        version = DecimalValue.TWO
      } else {
        version = `val`.asInstanceOf[DecimalValue]
      }
    }
  }

  /**
   * Get the numeric value of the version number appearing as an attribute on this element,
   * or inherited from its ancestors
   * @return the version number as a decimal
   */
  def getEffectiveVersion(): DecimalValue = {
    if (version == null) {
      val node = getParent
      if (node.isInstanceOf[StyleElement]) {
        version = node.asInstanceOf[StyleElement].getEffectiveVersion
      } else {
        return DecimalValue.TWO
      }
    }
    version
  }

  /**
   * Determine whether forwards-compatible mode is enabled for this element
   * @return true if forwards-compatible mode is enabled
   */
  def forwardsCompatibleModeIsEnabled(): Boolean = {
    getEffectiveVersion.compareTo(DecimalValue.TWO) > 0
  }

  /**
   * Determine whether 1.0-compatible mode is enabled for this element
   * @return true if 1.0 compatable mode is enabled, that is, if this or an enclosing
   *         element specifies an [xsl:]version attribute whose value is less than 2.0
   */
  def xPath10ModeIsEnabled(): Boolean = {
    getEffectiveVersion.compareTo(DecimalValue.TWO) < 0
  }

  /**
   * Process the [xsl:]default-xpath-namespace attribute if there is one
   * @param ns the namespace of the attribute required, either the XSLT namespace or ""
   * @throws XPathException if the value is invalid
   */
  protected def processDefaultCollationAttribute(ns: String): Unit = {
    val v = getAttributeValue(ns, "default-collation")
    if (v != null) {
      for (uri ← Whitespace.tokenize(v)) {
        if (uri == NamespaceConstant.CODEPOINT_COLLATION_URI || uri.startsWith("http://saxon.sf.net/")) {
          defaultCollationName = uri
          return
        } else {
          var collationURI: URI = null
          try {
            collationURI = new URI(uri, true)
            if (!collationURI.isAbsolute) {
              val base = new URI(getBaseURI)
              collationURI = base.resolve(collationURI.toString)
              uri = collationURI.toString
            }
          } catch {
            case err: URI.URISyntaxException ⇒ {
              compileError("default collation '" + uri + "' is not a valid URI")
              uri = NamespaceConstant.CODEPOINT_COLLATION_URI
            }
          }
          if (getConfiguration.getNamedCollation(uri) != null) {
            defaultCollationName = uri
            return
          }
        }
      }
      compileError("No recognized collation URI found in default-collation attribute", "XTSE0125")
    }
  }

  /**
   * Get the default collation for this stylesheet element. If no default collation is
   * specified in the stylesheet, return the Unicode codepoint collation name.
   * @return the name of the default collation
   */
  protected def getDefaultCollationName(): String = {
    var e = this
    while (e.isInstanceOf[StyleElement]) {
      if (e.asInstanceOf[StyleElement].defaultCollationName != null) {
        return e.asInstanceOf[StyleElement].defaultCollationName
      }
      e = e.getParent
    }
    NamespaceConstant.CODEPOINT_COLLATION_URI
  }

  /**
   * Check whether a particular extension element namespace is defined on this node.
   * This checks this node only, not the ancestor nodes.
   * The implementation checks whether the prefix is included in the
   * [xsl:]extension-element-prefixes attribute.
   * @param uri the namespace URI being tested
   * @return true if this namespace is defined on this element as an extension element namespace
   */
  protected def definesExtensionElement(uri: String): Boolean = {
    if (extensionNamespaces == null) {
      return false
    }
    for (extensionNamespace ← extensionNamespaces if extensionNamespace == uri) {
      return true
    }
    false
  }

  /**
   * Check whether a namespace uri defines an extension element. This checks whether the
   * namespace is defined as an extension namespace on this or any ancestor node.
   * @param uri the namespace URI being tested
   * @return true if the URI is an extension element namespace URI
   */
  def isExtensionNamespace(uri: String): Boolean = {
    val p = getParent
    definesExtensionElement(uri) || 
      (p.isInstanceOf[StyleElement] && 
      p.asInstanceOf[StyleElement].isExtensionNamespace(uri))
  }

  /**
   * Check whether this node excludes a particular namespace from the result.
   * This method checks this node only, not the ancestor nodes.
   * @param uri the namespace URI being tested
   * @return true if the namespace is excluded by virtue of an [xsl:]exclude-result-prefixes attribute
   */
  protected def definesExcludedNamespace(uri: String): Boolean = {
    if (excludedNamespaces == null) {
      return false
    }
    for (excludedNamespace ← excludedNamespaces if excludedNamespace == uri) {
      return true
    }
    false
  }

  /**
   * Check whether a namespace uri defines an namespace excluded from the result.
   * This checks whether the namespace is defined as an excluded namespace on this
   * or any ancestor node.
   * @param uri the namespace URI being tested
   * @return true if this namespace URI is a namespace excluded by virtue of exclude-result-prefixes
   *         on this element or on an ancestor element
   */
  def isExcludedNamespace(uri: String): Boolean = {
    if (uri == NamespaceConstant.XSLT || uri == NamespaceConstant.XML) {
      return true
    }
    val p = getParent
    definesExcludedNamespace(uri) || 
      (p.isInstanceOf[StyleElement] && 
      p.asInstanceOf[StyleElement].isExcludedNamespace(uri))
  }

  /**
   * Process the [xsl:]xpath-default-namespace attribute if there is one
   * @param ns the namespace URI of the attribute required  (the default namespace or the XSLT namespace.)
   */
  protected def processDefaultXPathNamespaceAttribute(ns: String): Unit = {
    val v = getAttributeValue(ns, "xpath-default-namespace")
    if (v != null) {
      defaultXPathNamespace = v
    }
  }

  /**
   * Get the default XPath namespace for elements and types
   * @return the default namespace for elements and types.
   *         Return [[NamespaceConstant#NULL]] for the non-namespace
   */
  protected def getDefaultXPathNamespace(): String = {
    var anc = this
    while (anc.isInstanceOf[StyleElement]) {
      val x = anc.asInstanceOf[StyleElement].defaultXPathNamespace
      if (x != null) {
        return x
      }
      anc = anc.getParent
    }
    NamespaceConstant.NULL
  }

  /**
   * Check that the stylesheet element is valid. This is called once for each element, after
   * the entire tree has been built. As well as validation, it can perform first-time
   * initialisation. The default implementation does nothing; it is normally overriden
   * in subclasses.
   * @param decl
   */
  def validate(decl: Declaration): Unit = {
  }

  /**
   * Hook to allow additional validation of a parent element immediately after its
   * children have been validated.
   */
  def postValidate(): Unit = {
  }

  /**
   * Method supplied by declaration elements to add themselves to a stylesheet-level index
   * @param decl the Declaration being indexed. (This corresponds to the StyleElement object
   * except in cases where one module is imported several times with different precedence.)
   * @param top  the outermost XSLStylesheet element
   */
  protected def index(decl: Declaration, top: PrincipalStylesheetModule): Unit = {
  }

  def typeCheck(exp: Expression): Expression = {
    if (exp == null) {
      return null
    }
    exp.setContainer(this)
    try {
      exp = makeExpressionVisitor().typeCheck(exp, Type.ITEM_TYPE)
      exp = ExpressionTool.resolveCallsToCurrentFunction(exp)
      if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
        val injector = LogController.getTraceListener.asInstanceOf[XSLTTraceListener]
          .getCodeInjector
        val name = ""
        exp = injector.inject(exp, getStaticContext, new StructuredQName("xsl", NamespaceConstant.XSLT, 
          "text"), new StructuredQName("", "", name))
      }
      exp
    } catch {
      case err: XPathException ⇒ if (err.isStaticError || err.isTypeError) {
        compileError(err)
        exp
      } else {
        val erexp = new ErrorExpression(err)
        erexp.setSourceLocator(this)
        erexp
      }
    }
  }

  /**
   * Allocate space for range variables within predicates in the match pattern. The xsl:template
   * element has no XPath expressions among its attributes, so if this method is called on this
   * object it can only be because there are variables used in the match pattern. We work out
   * how many slots are needed for the match pattern in each template rule, and then apply-templates
   * can allocate a stack frame that is large enough for the most demanding match pattern in the
   * entire stylesheet.
   * @param slots the number of slots required
   */
  def allocatePatternSlots(slots: Int): Unit = {
    getPrincipalStylesheetModule.allocatePatternSlots(slots)
  }

  /**
   * Type-check a pattern. This is called to check each pattern while the containing
   * instruction is being validated. It is not just a static type-check, it also adds code
   * to perform any necessary run-time type checking and/or conversion.
   * @param name    the name of the attribute holding the pattern, for example "match": used in
   *                diagnostics
   * @param pattern the compiled pattern
   * @return the original pattern, or a substitute pattern if it has been rewritten
   */
  def typeCheck(name: String, pattern: Pattern): Pattern = {
    if (pattern == null) {
      return null
    }
    try {
      pattern = pattern.analyze(makeExpressionVisitor(), Type.NODE_TYPE)
      var usesCurrent = false
      val sub = pattern.iterateSubExpressions()
      while (sub.hasNext) {
        val filter = sub.next().asInstanceOf[Expression]
        if (ExpressionTool.callsFunction(filter, Current.FN_CURRENT)) {
          usesCurrent = true
          //break
        }
      }
      if (usesCurrent) {
        val let = new LetExpression()
        let.setVariableQName(new StructuredQName("saxon", NamespaceConstant.SAXON, "current" + hashCode))
        let.setRequiredType(SequenceType.SINGLE_ITEM)
        let.setSequence(new ContextItemExpression())
        let.setAction(Literal.makeEmptySequence())
        val offer = new PromotionOffer()
        offer.action = PromotionOffer.REPLACE_CURRENT
        offer.containingExpression = let
        pattern.resolveCurrent(let, offer, true)
      }
      pattern
    } catch {
      case err: XPathException ⇒ if (err.isReportableStatically) {
        val e2 = new XPathException("Error in " + name + " pattern", err)
        e2.setLocator(this)
        e2.setErrorCodeQName(err.getErrorCodeQName)
        throw e2
      } else {
        val errpat = new LocationPathPattern()
        errpat.setExecutable(getExecutable)
        errpat.addFilter(new ErrorExpression(err))
        errpat
      }
    }
  }

  /**
   * Fix up references from XPath expressions. Overridden for function declarations
   * and variable declarations
   */
  def fixupReferences(): Unit = {
    for (child ← allChildren() if child.isInstanceOf[StyleElement]) {
      child.asInstanceOf[StyleElement].fixupReferences()
    }
  }

  /**
   * Recursive walk through the stylesheet to validate all nodes
   * @param decl
   */
  def validateSubtree(decl: Declaration): Unit = {
    if (isActionCompleted(StyleElement.ACTION_VALIDATE)) {
      return
    }
    setActionCompleted(StyleElement.ACTION_VALIDATE)
    if (validationError != null) {
      if (reportingCircumstances == REPORT_ALWAYS) {
        compileError(validationError)
      } else if (reportingCircumstances == REPORT_UNLESS_FORWARDS_COMPATIBLE && 
        !forwardsCompatibleModeIsEnabled()) {
        compileError(validationError)
      } else if (reportingCircumstances == REPORT_UNLESS_FALLBACK_AVAILABLE) {
        var hasFallback = false
        for (child ← allChildren() if child.isInstanceOf[XSLFallback]) {
          hasFallback = true
          child.asInstanceOf[XSLFallback].validateSubtree(decl)
        }
        if (!hasFallback) {
          compileError(validationError)
        }
      }
    } else {
      try {
        validate(decl)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
      validateChildren(decl)
      postValidate()
    }
  }

  /**
   * Validate the children of this node, recursively. Overridden for top-level
   * data elements.
   * @param decl
   */
  protected def validateChildren(decl: Declaration): Unit = {
    val containsInstructions = mayContainSequenceConstructor()
    for (child ← allChildren() if child.isInstanceOf[StyleElement]) {
      if (containsInstructions && !child.asInstanceOf[StyleElement].isInstruction && 
        !isPermittedChild(child.asInstanceOf[StyleElement])) {
        child.asInstanceOf[StyleElement].compileError("An " + getDisplayName + " element must not contain an " + 
          child.getDisplayName + 
          " element", "XTSE0010")
      }
      child.asInstanceOf[StyleElement].validateSubtree(decl)
    }
  }

  /**
   * Check whether a given child is permitted for this element. This method is used when a non-instruction
   * child element such as xsl:sort is encountered in a context where instructions would normally be expected.
   * @param child the child that may or may not be permitted
   * @return true if the child is permitted.
   */
  protected def isPermittedChild(child: StyleElement): Boolean = false

  /**
   * Get the PreparedStylesheet object.
   * @return the PreparedStylesheet to which this stylesheet element belongs.
   *         Exceptionally (with early errors in a simplified stylesheet module) return null.
   */
  def getPreparedStylesheet(): Executable = {
    val xss = getContainingStylesheet
    if (xss == null) null else xss.getPreparedStylesheet
  }

  /**
   * Get the principal stylesheet module
   * @return the principal stylesheet module
   */
  def getPrincipalStylesheetModule(): PrincipalStylesheetModule = {
    getContainingStylesheet.getPrincipalStylesheetModule
  }

  /**
   * Check that among the children of this element, any xsl:sort elements precede any other elements
   * @param sortRequired true if there must be at least one xsl:sort element
   * @throws XPathException if invalid
   */
  protected def checkSortComesFirst(sortRequired: Boolean): Unit = {
    var sortFound = false
    var nonSortFound = false
    for (child ← allChildren()) {
      if (child.isInstanceOf[XSLSort]) {
        if (nonSortFound) {
          child.asInstanceOf[XSLSort].compileError("Within " + getDisplayName + 
            ", xsl:sort elements must come before other instructions", "XTSE0010")
        }
        sortFound = true
      } else if (child.getNodeKind == Type.TEXT) {
        if (!Whitespace.isWhite(child.getStringValue)) {
          nonSortFound = true
        }
      } else {
        nonSortFound = true
      }
    }
    if (sortRequired && !sortFound) {
      compileError(getDisplayName + " must have at least one xsl:sort child", "XTSE0010")
    }
  }

  /**
   * Convenience method to check that the stylesheet element is at the top level
   * @param errorCode the error to throw if it is not at the top level; defaults to XTSE0010
   *                  if the value is null
   * @throws XPathException if not at top level
   */
  def checkTopLevel(errorCode: String): Unit = {
    if (!getParent.isInstanceOf[XSLStylesheet]) {
      compileError("Element must be used only at top level of stylesheet", if (errorCode == null) "XTSE0010" else errorCode)
    }
  }

  /**
   * Convenience method to check that the stylesheet element is empty
   * @throws XPathException if it is not empty
   */
  def checkEmpty(): Unit = {
    if (hasChildNodes()) {
      compileError("Element must be empty", "XTSE0260")
    }
  }

  /**
   * Validate the children against a list of allowed local names
   * @param localName the allowed local names, which must be in alphabetical order
   * @throws XPathException
   */
  def onlyAllow(localName: String*): Unit = {
    for (child ← allChildren()) {
      if (Arrays.binarySearch(localName, child.getLocalPart) >= 0) {
      } else if (child.getNodeKind == Type.TEXT) {
        if (!Whitespace.isWhite(child.getStringValue)) {
          compileError("No character data is allowed within " + getDisplayName, "XTSE0010")
        }
      } else {
        compileError("Child element " + child.getDisplayName + " is not allowed as a child of " + 
          getDisplayName, "XTSE0010")
      }
    }
  }

  /**
   * Convenience method to report the absence of a mandatory attribute
   * @param attribute the name of the attribute whose absence is to be reported
   * @throws XPathException if the attribute is missing
   */
  def reportAbsence(attribute: String): Unit = {
    compileError("Element must have an @" + attribute + " attribute", "XTSE0010")
  }

  /**
   * Compile the instruction on the stylesheet tree into an executable instruction
   * for use at run-time.
   * @param exec the Executable
   * @param decl the containing top-level declaration, for example xsl:function or xsl:template
   * @return either a ComputedExpression, or null. The value null is returned when compiling an instruction
   *         that returns a no-op, or when compiling a top-level object such as an xsl:template that compiles
   *         into something other than an instruction.
   */
  def compile(exec: Executable, decl: Declaration): Expression

  /**
   * Compile the children of this instruction on the stylesheet tree, adding the
   * subordinate instructions to the parent instruction on the execution tree.
   * @param exec          the Executable
   * @param decl          the Declaration of the containing top-level stylesheet element
   */
  def compileSequenceConstructor(exec: Executable, decl: Declaration): Expression = {
    compileSequenceConstructor(exec, decl, iterateAxis(Axis.CHILD, AnyNodeTest.getInstance))
  }

  private def compileSequenceConstructor(exec: Executable, decl: Declaration, iter: SequenceIterator): Expression = {
    val contents = new ArrayList[Expression](10)
    while (true) {
      val node = iter.next().asInstanceOf[NodeInfo]
      if (node == null) {
        //break
      }
      if (node.getNodeKind == Type.TEXT) {
        val lookahead = node.iterateAxis(Axis.FOLLOWING_SIBLING, AnyNodeTest.getInstance)
        val sibling = lookahead.next().asInstanceOf[NodeInfo]
        if (!(sibling.isInstanceOf[XSLParam] || sibling.isInstanceOf[XSLSort])) {
          var text = new ValueOf(new StringLiteral(node.getStringValue), false)
          text.setSourceLocator(this)
          if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
            val injector = LogController.getTraceListener.asInstanceOf[XSLTTraceListener]
              .getCodeInjector
            val tracer = injector.inject(text, getStaticContext, new StructuredQName("xsl", NamespaceConstant.XSLT, 
              "text"), null)
            tracer.setSourceLocator(this)
            if (tracer.isInstanceOf[Instruction]) {
              text = tracer.asInstanceOf[Instruction]
            }
          }
          contents.add(text)
        }
      } else if (node.isInstanceOf[XSLVariable]) {
        val `var` = node.asInstanceOf[XSLVariable].compileLocalVariable(exec, decl)
        if (`var` == null) {
        } else {
          val lv = `var`.asInstanceOf[LocalVariable]
          val tail = compileSequenceConstructor(exec, decl, iter)
          if (tail == null || Literal.isEmptySequence(tail)) {
          } else {
            val let = new LetExpression()
            let.setRequiredType(lv.getRequiredType)
            let.setVariableQName(lv.getVariableQName)
            let.setSequence(lv.getSelectExpression)
            let.setAction(tail)
            node.asInstanceOf[XSLVariable].fixupBinding(let)
            let.setSourceLocator(this)
            if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
              val t = new TraceExpression(let)
              t.setConstructType(Location.LET_EXPRESSION)
              t.setObjectName(lv.getVariableQName)
              contents.add(t)
            } else {
              contents.add(let)
            }
          }
        }
      } else if (node.isInstanceOf[StyleElement]) {
        val snode = node.asInstanceOf[StyleElement]
        var child: Expression = null
        if (snode.validationError != null && !this.isInstanceOf[AbsentExtensionElement]) {
          child = fallbackProcessing(exec, decl, snode)
        } else {
          child = snode.compile(exec, decl)
          if (child != null) {
            if (child.getContainer == null) {
              child.setContainer(this)
            }
            child.setSourceLocator(this)
            if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
              child = makeTraceInstruction(snode, child)
            }
          }
        }
        if (child != null) {
          contents.add(child)
        }
      }
    }
    val block = Block.makeBlock(contents)
    block.setSourceLocator(this)
    block
  }

  /**
   * Perform fallback processing. Generate fallback code for an extension
   * instruction that is not recognized by the implementation.
   * @param exec        the Executable
   * @param decl        the Declaration of the top-level element containing the extension instruction
   *@param instruction The unknown extension instruction @return the expression tree representing the fallback code
   */
  protected def fallbackProcessing(exec: Executable, decl: Declaration, instruction: StyleElement): Expression = {
    var fallback: Expression = null
    for (child ← instruction.allChildren() if child.isInstanceOf[XSLFallback]) {
      var b = child.asInstanceOf[XSLFallback].compileSequenceConstructor(exec, decl)
      if (b == null) {
        b = Literal.makeEmptySequence()
      }
      if (fallback == null) {
        fallback = b
      } else {
        fallback = Block.makeBlock(fallback, b)
        fallback.setSourceLocator(this)
      }
    }
    if (fallback != null) {
      fallback
    } else {
      new ErrorExpression(instruction.validationError)
    }
  }

  /**
   * Construct sort keys for a SortedIterator
   * @return an array of SortKeyDefinition objects if there are any sort keys;
   *         or null if there are none.
   * @param decl
   */
  protected def makeSortKeys(decl: Declaration): Array[SortKeyDefinition] = {
    var numberOfSortKeys = 0
    for (child ← allChildren() if child.isInstanceOf[XSLSort]) {
      child.asInstanceOf[XSLSort].compile(getExecutable, decl)
      if (numberOfSortKeys != 0 && child.asInstanceOf[XSLSort].getStable != null) {
        compileError("stable attribute may appear only on the first xsl:sort element", "XTSE1017")
      }
      numberOfSortKeys += 1
    }
    if (numberOfSortKeys > 0) {
      val keys = Array.ofDim[SortKeyDefinition](numberOfSortKeys)
      val k = 0
      for (child ← allChildren() if child.isInstanceOf[XSLSort]) {
        keys(k += 1) = child.asInstanceOf[XSLSort].getSortKeyDefinition.simplify(makeExpressionVisitor())
      }
      keys
    } else {
      null
    }
  }

  /**
   * Get the list of attribute-sets associated with this element.
   * This is used for xsl:element, xsl:copy, xsl:attribute-set, and on literal
   * result elements
   * @param use  the original value of the [xsl:]use-attribute-sets attribute
   * @param list an empty list to hold the list of XSLAttributeSet elements in the stylesheet tree.
   *             Or null, if these are not required.
   * @return an array of AttributeList instructions representing the compiled attribute sets
   * @throws XPathException if, for example, an attribute set name is an invalid QName
   */
  protected def getAttributeSets(use: String, list: List[Declaration]): Array[AttributeSet] = {
    if (list == null) {
      list = new ArrayList[Declaration](4)
    }
    val psm = getPrincipalStylesheetModule
    for (asetname ← Whitespace.tokenize(use)) {
      var name: StructuredQName = null
      try {
        name = makeQName(asetname)
      } catch {
        case err: NamespaceException ⇒ {
          compileError(err.getMessage, "XTSE0710")
          name = null
        }
        case err: XPathException ⇒ {
          compileError(err.getMessage, "XTSE0710")
          name = null
        }
      }
      val found = psm.getAttributeSets(name, list)
      if (!found) {
        compileError("No attribute-set exists named " + asetname, "XTSE0710")
      }
    }
    val array = Array.ofDim[AttributeSet](list.size)
    for (i ← 0 until list.size) {
      val aset = list.get(i).getSourceElement.asInstanceOf[XSLAttributeSet]
      array(i) = aset.getInstruction
    }
    array
  }

  /**
   * Get the list of xsl:with-param elements for a calling element (apply-templates,
   * call-template, apply-imports, next-match). This method can be used to get either
   * the tunnel parameters, or the non-tunnel parameters.
   * @param exec   the Executable
   * @param decl
   *@param tunnel true if the tunnel="yes" parameters are wanted, false to get
   * @param caller the calling instruction (for example xsl:apply-templates), used
   *               only for its location information @return an array of WithParam objects for either the ordinary parameters
   *         or the tunnel parameters
   */
  protected def getWithParamInstructions(exec: Executable, 
      decl: Declaration, 
      tunnel: Boolean, 
      caller: Expression): Array[WithParam] = {
    val params = new ArrayList[WithParam]()
    for (child ← allChildren() if child.isInstanceOf[XSLWithParam]) {
      val wp = child.asInstanceOf[XSLWithParam]
      if (wp.isTunnelParam == tunnel) {
        val p = wp.compile(exec, decl).asInstanceOf[WithParam]
        ExpressionTool.copyLocationInfo(caller, p)
        params.add(p)
      }
    }
    val array = Array.ofDim[WithParam](params.size)
    params.toArray(array)
  }

  /**
   * Report an error with diagnostic information
   * @param error contains information about the error
   * @throws XPathException always, after reporting the error to the ErrorListener
   */
  protected def compileError(error: XPathException): Unit = {
    error.setIsStaticError(true)
    if (error.getLocator == null) {
      error.setLocator(this)
    }
    val pss = getPreparedStylesheet
    if (pss == null) {
      throw error
    } else {
      pss.reportError(error)
    }
  }

  /**
   * Report a static error in the stylesheet
   * @param message the error message
   * @throws XPathException always, after reporting the error to the ErrorListener
   */
  protected def compileError(message: String): Unit = {
    val tce = new XPathException(message)
    tce.setLocator(this)
    compileError(tce)
  }

  /**
   * Compile time error, specifying an error code
   * @param message   the error message
   * @param errorCode the error code. May be null if not known or not defined
   * @throws XPathException
   */
  protected def compileError(message: String, errorCode: StructuredQName): Unit = {
    var tce: XPathException = null
    tce = if (LogConfiguration.loggingIsEnabled()) new XPathException(message) else new XPathException("")
    tce.setErrorCodeQName(errorCode)
    tce.setLocator(this)
    compileError(tce)
  }

  /**
   * Compile time error, specifying an error code
   * @param message   the error message
   * @param errorCode the error code. May be null if not known or not defined
   * @throws XPathException
   */
  protected def compileError(message: String, errorCode: String): Unit = {
    var tce: XPathException = null
    tce = if (LogConfiguration.loggingIsEnabled()) new XPathException(message) else new XPathException("")
    tce.setErrorCode(errorCode)
    tce.setLocator(this)
    compileError(tce)
  }

  protected def undeclaredNamespaceError(prefix: String, errorCode: String): Unit = {
    if (errorCode == null) {
      errorCode = "XTSE0280"
    }
    compileError("Undeclared namespace prefix " + Err.wrap(prefix), errorCode)
  }

  /**
   * Test whether this is a top-level element
   * @return true if the element is a child of the xsl:stylesheet element
   */
  def isTopLevel(): Boolean = getParent.isInstanceOf[XSLStylesheet]

  /**
   * Bind a variable used in this element to the compiled form of the XSLVariable element in which it is
   * declared
   * @param qName The name of the variable
   * @return the XSLVariableDeclaration (that is, an xsl:variable or xsl:param instruction) for the variable,
   *         or null if no declaration of the variable can be found
   */
  def bindVariable(qName: StructuredQName): XSLVariableDeclaration = {
    var curr = this
    var prev = this
    if (!isTopLevel) {
      var preceding = curr.iterateAxis(Axis.PRECEDING_SIBLING, NodeKindTest.ELEMENT)
      while (true) {
        curr = preceding.next().asInstanceOf[NodeInfo]
        while (curr == null) {
          curr = prev.getParent
          while (curr.isInstanceOf[StyleElement] && 
            !curr.asInstanceOf[StyleElement].seesAvuncularVariables()) {
            curr = curr.getParent
          }
          prev = curr
          if (curr.getParent.isInstanceOf[XSLStylesheet]) {
            //break
          }
          preceding = curr.iterateAxis(Axis.PRECEDING_SIBLING, NodeKindTest.ELEMENT)
          curr = preceding.next().asInstanceOf[NodeInfo]
        }
        if (curr.getParent.isInstanceOf[XSLStylesheet]) {
          //break
        }
        if (curr.isInstanceOf[XSLVariableDeclaration]) {
          val `var` = curr.asInstanceOf[XSLVariableDeclaration]
          if (`var`.getVariableQName == qName) {
            return `var`
          }
        }
      }
    }
    getPrincipalStylesheetModule.getGlobalVariable(qName)
  }

  /**
   * Ask whether variables declared in an "uncle" element are visible.
   * @return true for all elements except xsl:fallback and saxon:catch
   */
  protected def seesAvuncularVariables(): Boolean = true

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   * If there is no name, the value will be null.
   * @return the name of the object declared in this element, if any
   */
  def getObjectName(): StructuredQName = {
    if (objectName == null) {
      try {
        objectName = checkAttribute("name", "q").asInstanceOf[StructuredQName]
        if (objectName == null) {
          objectName = new StructuredQName("saxon", NamespaceConstant.SAXON, "unnamed-" + getLocalPart)
        }
      } catch {
        case err: XPathException ⇒ objectName = new StructuredQName("saxon", NamespaceConstant.SAXON,
          "unknown-" + getLocalPart)
      }
    }
    objectName
  }

  /**
   * Set the object name, for example the name of a function, variable, or template declared on this element
   * @param qName the object name as a QName
   */
  def setObjectName(qName: StructuredQName): Unit = {
    objectName = qName
  }

  /**
   * Get an iterator over all the properties available. The values returned by the iterator
   * will be of type String, and each string can be supplied as input to the getProperty()
   * method to retrieve the value of the property.
   */
  def getProperties(): Iterator[String] = {
    val list = new ArrayList[String](10)
    val it = iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
    while (true) {
      val a = it.next().asInstanceOf[NodeInfo]
      if (a == null) {
        //break
      }
      list.add(a.getNodeName.getClarkName)
    }
    list.iterator()
  }

  /**
   * Ask if an action on this StyleElement has been completed
   * @param action for example ACTION_VALIDATE
   * @return true if the action has already been performed
   */
  def isActionCompleted(action: Int): Boolean = (actionsCompleted & action) != 0

  /**
   * Say that an action on this StyleElement has been completed
   * @param action for example ACTION_VALIDATE
   */
  def setActionCompleted(action: Int): Unit = {
    actionsCompleted |= action
  }
}

package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.TraceExpression
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trace.Location
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.DocumentImpl
import client.net.sf.saxon.ce.tree.linked.LinkedTreeBuilder
import client.net.sf.saxon.ce.tree.util.NamespaceIterator
import com.google.gwt.logging.client.LogConfiguration
import java.util.ArrayList
import java.util.Iterator
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class represents a literal result element in the style sheet
 * (typically an HTML element to be output). <br>
 * It is also used to represent unknown top-level elements, which are ignored.
 */
class LiteralResultElement extends StyleElement {

  private var resultNameCode: StructuredQName = _

  private var attributeNames: Array[StructuredQName] = _

  private var attributeValues: Array[Expression] = _

  private var numberOfAttributes: Int = _

  private var toplevel: Boolean = _

  private var namespaceCodes: List[NamespaceBinding] = new ArrayList[NamespaceBinding]()

  private var attributeSets: Array[AttributeSet] = _

  private var inheritNamespaces: Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a sequence constructor
   * @return true: yes, it may contain a sequence constructor
   */
  def mayContainSequenceConstructor(): Boolean = true

  /**
   * Specify that this is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Process the attribute list
   */
  def prepareAttributes() {
    val atts = getAttributeList
    val num = atts.getLength
    if (num == 0) {
      numberOfAttributes = 0
    } else {
      attributeNames = Array.ofDim[StructuredQName](num)
      attributeValues = Array.ofDim[Expression](num)
      numberOfAttributes = 0
      for (i <- 0 until num) {
        val qn = atts.getStructuredQName(i)
        val uri = qn.getNamespaceURI
        val local = qn.getLocalName
        if (uri == NamespaceConstant.XSLT) {
          if (local == "use-attribute-sets") {
          } else if (local == "default-collation") {
          } else if (local == "extension-element-prefixes") {
          } else if (local == "exclude-result-prefixes") {
          } else if (local == "version") {
          } else if (local == "xpath-default-namespace") {
          } else if (local == "type" || 
            (local == "validation" && atts.getValue(i) != "strip")) {
            compileError("The xsl:type and xsl:validate attributes require a schema-aware processor", 
              "XTSE1660")
          } else if (local == "use-when") {
          } else if (local == "inherit-namespaces") {
            val inheritAtt = atts.getValue(i)
            if (inheritAtt == "yes") {
              inheritNamespaces = true
            } else if (inheritAtt == "no") {
              inheritNamespaces = false
            } else {
              compileError("The xsl:inherit-namespaces attribute has permitted values (yes, no)", "XTSE0020")
            }
          } else {
            compileError("Unknown XSL attribute " + qn.getDisplayName, "XTSE0805")
          }
        } else {
          attributeNames(numberOfAttributes) = qn
          val exp = makeAttributeValueTemplate(atts.getValue(i))
          attributeValues(numberOfAttributes) = exp
          numberOfAttributes += 1
        }
      }
      if (numberOfAttributes < attributeNames.length) {
        val attributeNames2 = Array.ofDim[StructuredQName](numberOfAttributes)
        System.arraycopy(attributeNames, 0, attributeNames2, 0, numberOfAttributes)
        attributeNames = attributeNames2
        val attributeValues2 = Array.ofDim[Expression](numberOfAttributes)
        System.arraycopy(attributeValues, 0, attributeValues2, 0, numberOfAttributes)
        attributeValues = attributeValues2
      }
    }
  }

  /**
   * Validate that this node is OK
   * @param decl
   */
  def validate(decl: Declaration) {
    toplevel = (getParent.isInstanceOf[XSLStylesheet])
    resultNameCode = getNodeName
    val elementURI = getURI
    if (toplevel) {
      if (elementURI.isEmpty) {
        compileError("Top level elements must have a non-null namespace URI", "XTSE0130")
      }
    } else {
      val inscope = NamespaceIterator.iterateNamespaces(this)
      while (inscope.hasNext) {
        namespaceCodes.add(inscope.next())
      }
      val resolver = new InscopeNamespaceResolver(this)
      val defaultNamespace = resolver.getURIForPrefix("", true)
      if (defaultNamespace.isEmpty) {
        namespaceCodes.add(NamespaceBinding.DEFAULT_UNDECLARATION)
      }
      val sheet = getPrincipalStylesheetModule
      if (sheet.hasNamespaceAliases()) {
        for (i <- 0 until namespaceCodes.size) {
          val suri = namespaceCodes.get(i).getURI
          val ncode = sheet.getNamespaceAlias(suri)
          if (ncode != null && ncode.getURI != suri) {
            namespaceCodes.set(i, ncode)
          }
        }
        val elementAlias = sheet.getNamespaceAlias(elementURI)
        if (elementAlias != null && elementAlias.getURI != elementURI) {
          resultNameCode = new StructuredQName(elementAlias.getPrefix, elementAlias.getURI, getLocalPart)
        }
      }
      val useAttSets = getAttributeValue(NamespaceConstant.XSLT, "use-attribute-sets")
      if (useAttSets != null) {
        attributeSets = getAttributeSets(useAttSets, null)
      }
      if (numberOfAttributes > 0) {
        for (i <- 0 until numberOfAttributes) {
          val anameCode = attributeNames(i)
          var alias = anameCode
          val attURI = anameCode.getNamespaceURI
          if (!attURI.isEmpty) {
            val newNSCode = sheet.getNamespaceAlias(attURI)
            if ((newNSCode != null && newNSCode.getURI != attURI)) {
              alias = new StructuredQName(newNSCode.getPrefix, newNSCode.getURI, getAttributeList.getLocalName(i))
            }
          }
          attributeNames(i) = alias
          attributeValues(i) = typeCheck(attributeValues(i))
        }
      }
      var n = namespaceCodes.size - 1
      while (n >= 0) {
        val uri = namespaceCodes.get(n).getURI
        if (isExcludedNamespace(uri) && !sheet.isAliasResultNamespace(uri)) {
          namespaceCodes.remove(n)
        }
        n -= 1
      }
    }
  }

  /**
   * Validate the children of this node, recursively. Overridden for top-level
   * data elements.
   * @param decl
   */
  protected def validateChildren(decl: Declaration) {
    if (!toplevel) {
      super.validateChildren(decl)
    }
  }

  /**
   * Compile code to process the literal result element at runtime
   */
  def compile(exec: Executable, decl: Declaration): Expression = {
    if (toplevel) return null
    val bindings = namespaceCodes.toArray(Array.ofDim[NamespaceBinding](namespaceCodes.size))
    val inst = new FixedElement(resultNameCode, bindings, inheritNamespaces)
    inst.setBaseURI(getBaseURI)
    var content = compileSequenceConstructor(exec, decl)
    if (numberOfAttributes > 0) {
      var i = attributeNames.length - 1
      while (i >= 0) {
        val att = new FixedAttribute(attributeNames(i))
        try {
          att.setSelect(attributeValues(i), exec.getConfiguration)
        } catch {
          case err: XPathException => compileError(err)
        }
        att.setSourceLocator(this)
        var exp = att
        if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
          val trace = new TraceExpression(exp)
          trace.setNamespaceResolver(new InscopeNamespaceResolver(this))
          trace.setConstructType(Location.LITERAL_RESULT_ATTRIBUTE)
          trace.setSourceLocator(this)
          trace.setObjectName(attributeNames(i))
          exp = trace
        }
        if (content == null) {
          content = exp
        } else {
          content = Block.makeBlock(exp, content)
          content.setSourceLocator(this)
        }
        i -= 1
      }
    }
    if (attributeSets != null) {
      val use = new UseAttributeSets(attributeSets)
      if (content == null) {
        content = use
      } else {
        content = Block.makeBlock(use, content)
        content.setSourceLocator(this)
      }
    }
    if (content == null) {
      content = Literal.makeEmptySequence()
    }
    inst.setContentExpression(content)
    inst
  }

  /**
   * Make a top-level literal result element into a stylesheet. This implements
   * the "Simplified Stylesheet" facility.
   * @param exec the PreparedStylesheet (the compiled stylesheet as provided)
   * @return the reconstructed stylesheet with an xsl:stylesheet and xsl:template element added
   */
  def makeStylesheet(exec: Executable): DocumentImpl = {
    val nodeFactory = new StyleNodeFactory(exec.getConfiguration)
    val xslPrefix = getPrefixForURI(NamespaceConstant.XSLT)
    if (xslPrefix == null) {
      var message: String = null
      message = if (getLocalPart == "stylesheet" || getLocalPart == "transform") if (getPrefixForURI(NamespaceConstant.MICROSOFT_XSL) != null) "Saxon is not able to process Microsoft's WD-xsl dialect" else "Namespace for stylesheet element should be " + NamespaceConstant.XSLT else if (getLocalPart == "parsererror") "Stylesheet is not well-formed XML. " + getStringValue else "The supplied file does not appear to be a stylesheet (found " + 
        getLocalPart + 
        ")"
      val err = new XPathException(message, "XTSE0150")
      err.setLocator(this)
      err.setIsStaticError(true)
      exec.reportError(err)
      throw err
    }
    val version = getAttributeValue(NamespaceConstant.XSLT, "version")
    if (version == null) {
      val err = new XPathException("Simplified stylesheet: xsl:version attribute is missing", "XTSE0150")
      err.setIsStaticError(true)
      err.setLocator(this)
      exec.reportError(err)
      throw err
    }
    try {
      val builder = new LinkedTreeBuilder()
      builder.setPipelineConfiguration(exec.getConfiguration.makePipelineConfiguration())
      builder.setNodeFactory(nodeFactory)
      builder.setSystemId(this.getSystemId)
      builder.open()
      builder.startDocument()
      val st = new StructuredQName("xsl", NamespaceConstant.XSLT, "stylesheet")
      builder.startElement(st, 0)
      builder.namespace(new NamespaceBinding("xsl", NamespaceConstant.XSLT), 0)
      builder.attribute(new StructuredQName("", "", "version"), version)
      builder.startContent()
      val te = new StructuredQName("xsl", NamespaceConstant.XSLT, "template")
      builder.startElement(te, 0)
      builder.attribute(new StructuredQName("", "", "match"), "/")
      builder.startContent()
      builder.graftElement(this)
      builder.endElement()
      builder.endElement()
      builder.endDocument()
      builder.close()
      builder.getCurrentRoot.asInstanceOf[DocumentImpl]
    } catch {
      case err: XPathException => {
        err.setLocator(this)
        throw err
      }
    }
  }
}

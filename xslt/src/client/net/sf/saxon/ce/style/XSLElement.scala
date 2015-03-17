package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.StringLiteral
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.EmptySequence
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:element element in the stylesheet. <br>
 */
class XSLElement extends StyleElement {

  private var elementName: Expression = _

  private var namespace: Expression = null

  private var use: String = _

  private var attributeSets: Array[AttributeSet] = null

  private var inheritNamespaces: Boolean = true

  /**
   * Determine whether this node is an instruction.
   *
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   *
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes() {
    elementName = checkAttribute("name", "a1").asInstanceOf[Expression]
    namespace = checkAttribute("namespace", "a").asInstanceOf[Expression]
    checkAttribute("validation", "v")
    checkAttribute("type", "t")
    val b = checkAttribute("inherit-namespaces", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      inheritNamespaces = b
    }
    use = checkAttribute("use-attribute-sets", "s").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    if (use != null) {
      attributeSets = getAttributeSets(use, null)
    }
    elementName = typeCheck(elementName)
    namespace = typeCheck(namespace)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val resolver = new InscopeNamespaceResolver(this)
    if (elementName.isInstanceOf[StringLiteral]) {
      val qName = elementName.asInstanceOf[StringLiteral].getStringValue
      var parts: Array[String] = null
      try {
        parts = NameChecker.getQNameParts(qName)
      } catch {
        case e: QNameException => {
          compileError("Invalid element name: " + qName, "XTDE0820")
          return null
        }
      }
      var nsuri: String = null
      if (namespace.isInstanceOf[StringLiteral]) {
        nsuri = namespace.asInstanceOf[StringLiteral].getStringValue
        if (nsuri.length == 0) {
          parts(0) = ""
        }
      } else if (namespace == null) {
        nsuri = resolver.getURIForPrefix(parts(0), true)
        if (nsuri == null) {
          undeclaredNamespaceError(parts(0), "XTDE0830")
        }
      }
      if (nsuri != null) {
        val nameCode = new StructuredQName(parts(0), nsuri, parts(1))
        val inst = new FixedElement(nameCode, null, inheritNamespaces)
        inst.setBaseURI(getBaseURI)
        if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
          inst.AddTraceProperty("name", elementName)
        }
        return compileContentExpression(exec, decl, inst)
      }
    }
    val inst = new ComputedElement(elementName, namespace, resolver, inheritNamespaces)
    compileContentExpression(exec, decl, inst)
  }

  private def compileContentExpression(exec: Executable, decl: Declaration, inst: ElementCreator): Expression = {
    var content = compileSequenceConstructor(exec, decl)
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
      content = new Literal(EmptySequence.getInstance)
    }
    inst.setContentExpression(content)
    inst
  }
}

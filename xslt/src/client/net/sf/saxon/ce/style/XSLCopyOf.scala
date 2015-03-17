package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.CopyOf
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:copy-of element in the stylesheet. <br>
 */
class XSLCopyOf extends StyleElement {

  private var select: Expression = _

  private var copyNamespaces: Boolean = _

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  def prepareAttributes() {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    val b = checkAttribute("copy-namespaces", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      copyNamespaces = b
    }
    checkAttribute("type", "t")
    checkAttribute("validation", "v")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    checkEmpty()
    select = typeCheck(select)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val inst = new CopyOf(select, copyNamespaces)
    inst.setStaticBaseUri(getBaseURI)
    inst
  }
}

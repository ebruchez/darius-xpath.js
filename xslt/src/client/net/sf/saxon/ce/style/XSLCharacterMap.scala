package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:character-map declaration in the stylesheet. <br>
 */
class XSLCharacterMap extends StyleElement {

  var use: String = _

  var validated: Boolean = false

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  /**
   * Validate the attributes on this instruction
   * @throws XPathException
   */
  def prepareAttributes() {
    setObjectName(checkAttribute("name", "q1").asInstanceOf[StructuredQName])
    use = checkAttribute("use-character-maps", "s").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    if (validated) return
    checkTopLevel(null)
    onlyAllow("output-character")
    validated = true
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}

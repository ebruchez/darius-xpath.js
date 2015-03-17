package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Compile-time representation of an xsl:import-schema declaration
 * in a stylesheet
 */
class XSLImportSchema extends StyleElement {

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes() {
    checkAttribute("schema-location", "s")
    checkAttribute("namespace", "s")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    checkTopLevel(null)
    compileError("This XSLT processor is not schema-aware")
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}

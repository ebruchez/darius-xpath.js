package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.ApplyImports
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:next-match element in the stylesheet
 */
class XSLNextMatch extends StyleElement {

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  def prepareAttributes() {
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    onlyAllow("fallback", "with-param")
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val inst = new ApplyImports(ApplyImports.NEXT_MATCH)
    inst.setActualParameters(getWithParamInstructions(exec, decl, false, inst), getWithParamInstructions(exec, 
      decl, true, inst))
    inst
  }
}

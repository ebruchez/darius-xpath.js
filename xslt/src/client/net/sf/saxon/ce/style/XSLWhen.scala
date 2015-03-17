// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:when elements in stylesheet. <br>
 * The xsl:while element has a mandatory attribute test, a boolean expression.
 */
class XSLWhen extends StyleElement {

  private var test: Expression = _

  def getCondition(): Expression = test

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  def prepareAttributes() {
    test = checkAttribute("test", "e1").asInstanceOf[Expression]
    checkForUnknownAttributes()
  }

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def validate(decl: Declaration) {
    if (!(getParent.isInstanceOf[XSLChoose])) {
      compileError("xsl:when must be immediately within xsl:choose", "XTSE0010")
    }
    test = typeCheck(test)
  }

  /**
   * Mark tail-recursive calls on stylesheet functions. For most instructions, this does nothing.
   */
  def markTailCalls(): Boolean = {
    val last = getLastChildInstruction
    last != null && last.markTailCalls()
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}

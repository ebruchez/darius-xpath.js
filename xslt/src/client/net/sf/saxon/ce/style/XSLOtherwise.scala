// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:otherwise elements in stylesheet. <br>
 */
class XSLOtherwise extends StyleElement {

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  def prepareAttributes(): Unit = {
    checkForUnknownAttributes()
  }

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def validate(decl: Declaration): Unit = {
    if (!getParent.isInstanceOf[XSLChoose]) {
      compileError("xsl:otherwise must be immediately within xsl:choose", "XTSE0010")
    }
  }

  /**
   * Mark tail-recursive calls on stylesheet functions. For most instructions, this does nothing.
   */
  def markTailCalls(): Boolean = {
    val last = getLastChildInstruction
    last != null && last.markTailCalls()
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    throw new UnsupportedOperationException("XSLOtherwise#compile() should not be called")
  }
}

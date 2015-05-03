// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.Literal
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.ValueOf
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.linked.NodeImpl
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:text elements in stylesheet. <BR>
 */
class XSLText extends XSLLeafNodeConstructor {

  private var value: StringValue = _

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = NodeKindTest.TEXT

  def prepareAttributes(): Unit = {
    checkAttribute("disable-output-escaping", "b")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    value = StringValue.EMPTY_STRING
    for (child ← allChildren()) {
      if (child.isInstanceOf[StyleElement]) {
        child.asInstanceOf[StyleElement].compileError("xsl:text must not contain child elements", "XTSE0010")
        return
      } else {
        value = StringValue.makeStringValue(child.getStringValue)
      }
    }
    super.validate(decl)
  }

  /**
   * Get the error code to be returned when the element has a select attribute but is not empty.
   * @return the error code defined for this condition, for this particular instruction
   */
  protected def getErrorCodeForSelectPlusContent(): String = null

  def compile(exec: Executable, decl: Declaration): Expression = {
    new ValueOf(Literal.makeLiteral(value), false)
  }
}

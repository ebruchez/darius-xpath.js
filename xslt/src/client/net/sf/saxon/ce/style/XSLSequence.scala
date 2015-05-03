// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.LogController
import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.ItemType
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:sequence element in the stylesheet. <br>
 * The xsl:sequence element takes attributes:<ul>
 * <li>a mandatory attribute select="expression".</li>
 * </ul>
 */
class XSLSequence extends StyleElement {

  private var select: Expression = _

  private var selectAttTrace: String = ""

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = select.getItemType

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = false

  def prepareAttributes(): Unit = {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    checkForUnknownAttributes()
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      selectAttTrace = getAttributeValue("", "select")
    }
  }

  def validate(decl: Declaration): Unit = {
    onlyAllow("fallback")
    select = typeCheck(select)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      select.AddTraceProperty("select", selectAttTrace)
    }
    select
  }
}

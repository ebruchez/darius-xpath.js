// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.StringLiteral
import org.orbeon.darius.xpath.expr.instruct.Block
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.Message
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:message element in the stylesheet. <br>
 */
class XSLMessage extends StyleElement {

  private var terminate: Expression = null

  private var select: Expression = null

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes(): Unit = {
    terminate = checkAttribute("terminate", "a").asInstanceOf[Expression]
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    if (terminate == null) {
      terminate = makeAttributeValueTemplate("no")
    }
  }

  def validate(decl: Declaration): Unit = {
    select = typeCheck(select)
    terminate = typeCheck(terminate)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val b = compileSequenceConstructor(exec, decl)
    if (b != null) {
      if (select == null) {
        select = b
      } else {
        select = Block.makeBlock(select, b)
        select.setSourceLocator(this)
      }
    }
    if (select == null) {
      select = new StringLiteral("xsl:message (no content)")
    }
    val inst = new Message(select, terminate)
    inst
  }
}

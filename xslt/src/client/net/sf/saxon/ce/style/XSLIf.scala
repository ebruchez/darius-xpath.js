// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionTool
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.Choose
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:if elements in stylesheet. <br>
 * The xsl:if element has a mandatory attribute test, a boolean expression.
 * The content is output if the test condition is true.
 */
class XSLIf extends StyleElement {

  private var test: Expression = _

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
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes(): Unit = {
    test = checkAttribute("test", "e1").asInstanceOf[Expression]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    test = typeCheck(test)
  }

  /**
   * Mark tail-recursive calls on stylesheet functions. For most instructions, this does nothing.
   */
  def markTailCalls(): Boolean = {
    val last = getLastChildInstruction
    last != null && last.markTailCalls()
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    if (test.isInstanceOf[Literal]) {
      val testVal = test.asInstanceOf[Literal].getValue
      try {
        if (ExpressionTool.effectiveBooleanValue(testVal.iterate())) {
          return compileSequenceConstructor(exec, decl)
        } else {
          return null
        }
      } catch {
        case err: XPathException => 
      }
    }
    val action = compileSequenceConstructor(exec, decl)
    if (action == null) {
      return null
    }
    val conditions = Array(test)
    val actions = Array(action)
    val inst = new Choose(conditions, actions)
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      inst.AddTraceProperty("test", getAttributeValue("", "test"))
    }
    inst
  }
}

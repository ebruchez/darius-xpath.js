// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr.Container
import client.net.sf.saxon.ce.expr.ExpressionTool
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A compiled global variable in a stylesheet or query. <br>
 */
class GlobalVariable extends GeneralVariable with Container {

  @BeanProperty
  var executable: Executable = _

  private var numberOfSlots: Int = 0

  /**
   * Get the granularity of the container.
   * @return 0 for a temporary container created during parsing; 1 for a container
   *         that operates at the level of an XPath expression; 2 for a container at the level
   *         of a global function or template
   */
  def getContainerGranularity(): Int = 2

  /**
   * The expression that initializes a global variable may itself use local variables.
   * In this case a stack frame needs to be allocated while evaluating the global variable
   * @param numberOfSlots The space needed for local variables used while evaluating this global
   * variable.
   */
  def setContainsLocals(numberOfSlots: Int): Unit = {
    this.numberOfSlots = numberOfSlots
  }

  /**
   * Is this a global variable?
   * @return true (yes, it is a global variable)
   */
  def isGlobal(): Boolean = true

  /**
   * Process the variable declaration
   */
  def processLeavingTail(context: XPathContext): TailCall = null

  /**
   * Evaluate the variable. That is,
   * get the value of the select expression if present or the content
   * of the element otherwise, either as a tree or as a sequence
   */
  def getSelectValue(context: XPathContext): Sequence = {
    if (select == null) {
      throw new AssertionError("*** No select expression for global variable $" + getVariableQName.getDisplayName + 
        "!!")
    } else {
      val c2 = context.newCleanContext()
      c2.setSingletonFocus(c2.getController.getContextForGlobalVariables)
      if (numberOfSlots != 0) {
        c2.openStackFrame(numberOfSlots)
      }
      ExpressionTool.evaluate(select, evaluationMode, c2)
    }
  }

  /**
   * Evaluate the variable
   */
  def evaluateVariable(context: XPathContext): Sequence = {
    val controller = context.getController
    val b = controller.getBindery
    val v = b.getGlobalVariable(getSlotNumber)
    if (v != null) {
      v
    } else {
      actuallyEvaluate(context)
    }
  }

  /**
   * Evaluate the global variable, and save its value for use in subsequent references.
   * @param context the XPath dynamic context
   * @return the value of the variable
   * @throws XPathException if evaluation fails
   */
  protected def actuallyEvaluate(context: XPathContext): Sequence = {
    val controller = context.getController
    val b = controller.getBindery
    try {
      val go = b.setExecuting(this)
      if (!go) {
        return b.getGlobalVariable(getSlotNumber)
      }
      val value = getSelectValue(context)
      b.saveGlobalVariableValue(this, value)
    } catch {
      case err: XPathException ⇒ {
        b.setNotExecuting(this)
        if (err.isInstanceOf[XPathException.Circularity]) {
          err.setErrorCode("XTDE0640")
          err.setLocator(getSourceLocator)
          throw err
        } else {
          throw err
        }
      }
    }
  }
}

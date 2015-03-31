// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.Choose
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.BooleanValue
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:choose element in the stylesheet. <br>
 */
class XSLChoose extends StyleElement {

  private var otherwise: StyleElement = _

  private var numberOfWhens: Int = 0

  /**
   * Determine whether this node is an instruction.
   *
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   *
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  def prepareAttributes(): Unit = {
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    for (child ← allChildren()) {
      if (child.isInstanceOf[XSLWhen]) {
        if (otherwise != null) {
          otherwise.compileError("xsl:otherwise must come last", "XTSE0010")
        }
        numberOfWhens += 1
      } else if (child.isInstanceOf[XSLOtherwise]) {
        if (otherwise != null) {
          child.asInstanceOf[XSLOtherwise].compileError("Only one xsl:otherwise is allowed in an xsl:choose", 
            "XTSE0010")
        } else {
          otherwise = child.asInstanceOf[StyleElement]
        }
      } else {
        val se = if (child.isInstanceOf[StyleElement]) child.asInstanceOf[StyleElement] else this
        se.compileError("Only xsl:when and xsl:otherwise are allowed within xsl:choose", "XTSE0010")
      }
    }
    if (numberOfWhens == 0) {
      compileError("xsl:choose must contain at least one xsl:when", "XTSE0010")
    }
  }

  /**
   * Mark tail-recursive calls on templates and functions.
   */
  def markTailCalls(): Boolean = {
    var found = false
    for (child ← allChildren()) {
      found |= child.asInstanceOf[StyleElement].markTailCalls()
    }
    found
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val entries = numberOfWhens + (if (otherwise == null) 0 else 1)
    val conditions = Array.ofDim[Expression](entries)
    val actions = Array.ofDim[Expression](entries)
    var conditionTests: Array[String] = null
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      conditionTests = Array.ofDim[String](entries)
    }
    var w = 0
    val visitor = makeExpressionVisitor()
    for (child ← allChildren()) {
      val action = child.asInstanceOf[StyleElement].compileSequenceConstructor(exec, decl)
      actions(w) = visitor.simplify(action)
      if (child.isInstanceOf[XSLWhen]) {
        conditions(w) = child.asInstanceOf[XSLWhen].getCondition
      } else if (child.isInstanceOf[XSLOtherwise]) {
        conditions(w) = Literal.makeLiteral(BooleanValue.TRUE)
      } else {
      }
      if (conditionTests != null) {
        conditionTests(w) = if (child.isInstanceOf[XSLWhen]) child.asInstanceOf[XSLWhen].getAttributeValue("",
          "test")
        else ""
      }
      w += 1
    }
    val ch = new Choose(conditions, actions)
    ch.setConditionTests(conditionTests)
    ch
  }
}

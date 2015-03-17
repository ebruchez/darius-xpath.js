// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{SequenceExtent, SequenceTool}

/**
 * A TailCallLoop wraps the body of a function that contains tail-recursive function calls. On completion
 * of the "real" body of the function it tests whether the function has executed a tail call, and if so,
 * iterates to evaluate the tail call.
 */
class TailCallLoop(var containingFunction: UserFunction) extends UnaryExpression(function.getBody) {

  /**
   * Get the containing function
   * @return the containing function
   */
  def getContainingFunction(): UserFunction = containingFunction

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    this
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod(): Int = operand.getImplementationMethod

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  private def copy(): Expression = {
    throw new UnsupportedOperationException("TailCallLoop.copy()")
  }

  /**
   * Iterate over the sequence of values
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val cm = context
    while (true) {
      val iter = operand.iterate(cm)
      val extent = SequenceExtent.makeSequenceExtent(iter)
      val fn = cm.getTailCallFunction
      if (fn == null) {
        return extent.iterate()
      }
      if (fn != containingFunction) {
        tailCallDifferentFunction(fn, cm).iterate()
      }
    }
  }

  /**
   * Evaluate as an Item.
   */
  def evaluateItem(context: XPathContext): Item = {
    val cm = context
    while (true) {
      val item = operand.evaluateItem(context)
      val fn = cm.getTailCallFunction
      if (fn == null) {
        return item
      }
      if (fn != containingFunction) {
        SequenceTool.asItem(tailCallDifferentFunction(fn, cm))
      }
    }
  }

  /**
   * Process the function body
   * @param context The dynamic context, giving access to the current node,
   *                the current variables, etc.
   */
  def process(context: XPathContext) {
    val cm = context
    while (true) {
      operand.process(context)
      val fn = cm.getTailCallFunction
      if (fn == null) {
        return
      }
      if (fn != containingFunction) {
        SequenceTool.process(tailCallDifferentFunction(fn, cm).iterate(), cm)
        return
      }
    }
  }

  /**
   * Make a tail call on a different function. This reuses the context object and the stack frame array
   * where possible, but it does consume some Java stack space. It's still worth it, because we don't use
   * as much stack as we would if we didn't return down to the TailCallLoop level.
   * @param fn the function to be called
   * @param cm the dynamic context
   * @return the result of calling the other function
   * @throws XPathException if the called function fails
   */
  private def tailCallDifferentFunction(fn: UserFunction, cm: XPathContext): Sequence = {
    cm.resetStackFrameMap(fn.getNumberOfSlots, fn.getNumberOfArguments)
    try {
      ExpressionTool.evaluate(fn.getBody, fn.getEvaluationMode, cm)
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
  }

  /**
   * Determine the data type of the items returned by the expression
   */
  def getItemType(): ItemType = operand.getItemType
}

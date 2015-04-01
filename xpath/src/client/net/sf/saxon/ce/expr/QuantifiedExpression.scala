// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType}
import client.net.sf.saxon.ce.functions.BooleanFn
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.BooleanValue

import scala.beans.BeanProperty

/**
 * A QuantifiedExpression tests whether some/all items in a sequence satisfy
 * some condition.
 */
class QuantifiedExpression extends Assignation {

  @BeanProperty
  var operator: Int = _

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    sequence = visitor.typeCheck(sequence, contextItemType)
    if (Literal.isEmptySequence(sequence)) {
      return Literal.makeLiteral(BooleanValue.get(operator != Token.SOME))
    }
    val config = visitor.getConfiguration
    sequence = ExpressionTool.unsorted(config, sequence, false)
    val actualItemType = sequence.getItemType
    refineTypeInformation(actualItemType, StaticProperty.EXACTLY_ONE, null, sequence.getSpecialProperties, 
      visitor, this)
    action = visitor.typeCheck(action, contextItemType)
    val err = TypeChecker.ebvError(action)
    if (err != null) {
      err.setLocator(this.getSourceLocator)
      throw err
    }
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val config = visitor.getConfiguration
    sequence = visitor.optimize(sequence, contextItemType)
    action = visitor.optimize(action, contextItemType)
    val ebv = BooleanFn.rewriteEffectiveBooleanValue(action, visitor, contextItemType)
    if (ebv != null) {
      action = ebv
      adoptChildExpression(ebv)
    }
    val offer = new PromotionOffer()
    offer.containingExpression = this
    offer.action = PromotionOffer.RANGE_INDEPENDENT
    offer.bindingList = Array(this)
    action = doPromotion(action, offer)
    if (offer.containingExpression.isInstanceOf[LetExpression]) {
      offer.containingExpression = visitor.optimize(visitor.typeCheck(offer.containingExpression, contextItemType), 
        contextItemType)
    }
    val e2 = offer.containingExpression
    if (e2 != this) {
      return e2
    }
    this
  }

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  override def hasLoopingSubexpression(child: Expression): Boolean = child == action

  /**
   * Determine the special properties of this expression
   * @return [[StaticProperty.NON_CREATIVE]].
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Evaluate the expression to return a singleton value
   */
  override def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Get the result as a boolean
   */
  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val base = sequence.iterate(context)
    val some = operator == Token.SOME
    val slot = getLocalSlotNumber
    while (true) {
      val it = base.next()
      if (it == null) {
        return ! some
      }
      context.setLocalVariable(slot, it)
      if (some == action.effectiveBooleanValue(context)) {
        return some
      }
    }
    throw new IllegalStateException
  }

  /**
   * Determine the data type of the items returned by the expression
   * @return Type.BOOLEAN
   */
  def getItemType: ItemType = AtomicType.BOOLEAN

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   * @return a representation of the expression as a string
   */
  override def toString: String = {
    (if (operator == Token.SOME) "some" else "every") + " $" + 
      getVariableName + 
      " in " + 
      sequence.toString + 
      " satisfies " + 
      action.toString
  }
}

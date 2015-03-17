// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType}
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{AtomicValue, BooleanValue, NumericValue}

/**
 * An IntegerRangeTest is an expression of the form
 * E = N to M
 * where E is numeric, and N and M are both expressions of type integer.
 */
class IntegerRangeTest(var value: Expression, var min: Expression, var max: Expression)
    extends Expression {

  /**
   * Get the value to be tested
   * @return the expression that evaluates to the value being tested
   */
  def getValueExpression(): Expression = value

  /**
   * Get the expression denoting the start of the range
   * @return the expression denoting the minumum value
   */
  def getMinValueExpression(): Expression = min

  /**
   * Get the expression denoting the end of the range
   * @return the expression denoting the maximum value
   */
  def getMaxValueExpression(): Expression = max

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

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
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  /**
   * Get the data type of the items returned
   */
  def getItemType(): ItemType = AtomicType.BOOLEAN

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Get the immediate sub-expressions of this expression. Default implementation
   * returns a zero-length array, appropriate for an expression that has no
   * sub-expressions.
   *
   * @return an iterator containing the sub-expressions of this expression
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(value, min, max)

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      if (offer.action != PromotionOffer.UNORDERED) {
        value = doPromotion(value, offer)
        min = doPromotion(min, offer)
        max = doPromotion(max, offer)
      }
      this
    }
  }

  /**
   * Evaluate the expression
   */
  override def evaluateItem(c: XPathContext): Item = {
    val av = value.evaluateItem(c).asInstanceOf[AtomicValue]
    if (av == null) {
      return BooleanValue.FALSE
    }
    val v = av.asInstanceOf[NumericValue]
    if (!v.isWholeNumber) {
      return BooleanValue.FALSE
    }
    val av2 = min.evaluateItem(c).asInstanceOf[AtomicValue]
    val v2 = av2.asInstanceOf[NumericValue]
    if (v.compareTo(v2) < 0) {
      return BooleanValue.FALSE
    }
    val av3 = max.evaluateItem(c).asInstanceOf[AtomicValue]
    val v3 = av3.asInstanceOf[NumericValue]
    BooleanValue.get(v.compareTo(v3) <= 0)
  }
}

package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.Cardinality
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

abstract class SingleItemFilter extends UnaryExpression {

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expresion visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        {@link client.net.sf.saxon.ce.type.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws client.net.sf.saxon.ce.trans.XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.optimize(operand, contextItemType)
    if (!Cardinality.allowsMany(operand.getCardinality)) {
      return operand
    }
    super.optimize(visitor, contextItemType)
  }

  /**
   * Promote this expression if possible
   */
  def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      if (offer.action != PromotionOffer.UNORDERED) {
        operand = doPromotion(operand, offer)
      }
      this
    }
  }

  /**
   * Get the static cardinality: this implementation is appropriate for [1] and [last()] which will always
   * return something if the input is non-empty
   */
  def computeCardinality(): Int = {
    operand.getCardinality & ~StaticProperty.ALLOWS_MANY
  }
}

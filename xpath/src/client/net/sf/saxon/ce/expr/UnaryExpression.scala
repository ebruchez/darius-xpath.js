// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceExtent

/**
 * Unary Expression: an expression taking a single operand expression
 */
abstract class UnaryExpression protected () extends Expression {

  protected var operand: Expression = _

  def this(p0: Expression) {
    this()
    operand = p0
    adoptChildExpression(p0)
  }

  def getBaseExpression(): Expression = operand

  /**
   * Simplify an expression
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    this
  }

  /**
   * Type-check the expression. Default implementation for unary operators that accept
   * any kind of operand
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
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
    operand = visitor.optimize(operand, contextItemType)
    try {
      if (operand.isInstanceOf[Literal]) {
        return Literal.makeLiteral(SequenceExtent.makeSequenceExtent(iterate(new EarlyEvaluationContext(visitor.getConfiguration))))
      }
    } catch {
      case err: XPathException ⇒
    }
    this
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      operand = doPromotion(operand, offer)
      this
    }
  }

  /**
   * Get the immediate subexpressions of this expression
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(operand)

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = operand.getSpecialProperties

  /**
   * Determine the static cardinality. Default implementation returns the cardinality of the operand
   */
  def computeCardinality(): Int = operand.getCardinality

  /**
   * Determine the data type of the expression, if possible. The default
   * implementation for unary expressions returns the item type of the operand
   * @return the item type of the items in the result sequence, insofar as this
   * is known statically.
   */
  def getItemType(): ItemType = operand.getItemType

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    other != null && this.getClass == other.getClass && 
      this.operand == other.asInstanceOf[UnaryExpression].operand
  }

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = {
    ("UnaryExpression " + getClass).hashCode ^ operand.hashCode
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = {
    getClass.getName + "(" + operand.toString + ")"
  }
}

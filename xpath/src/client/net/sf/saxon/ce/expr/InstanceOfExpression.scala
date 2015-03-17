// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{BooleanValue, Cardinality, SequenceType}

/**
 * InstanceOf Expression: implements "Expr instance of data-type"
 */
class InstanceOfExpression(source: Expression, var targetType: SequenceType)
    extends UnaryExpression(source) {

  /**
   * Get the item type that we are testing for membership of
   * @return the item type
   */
  def getRequiredItemType(): ItemType = targetType.getPrimaryType

  /**
   * Type-check the expression
   * @return the checked expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    if (operand.isInstanceOf[Literal]) {
      val lit = Literal.makeLiteral(evaluateItem(new EarlyEvaluationContext(visitor.getConfiguration)))
      ExpressionTool.copyLocationInfo(this, lit)
      return lit
    }
    if (Cardinality.subsumes(targetType.getCardinality, operand.getCardinality)) {
      val th = TypeHierarchy.getInstance
      val relation = th.relationship(operand.getItemType, targetType.getPrimaryType)
      if (relation == TypeHierarchy.SAME_TYPE || relation == TypeHierarchy.SUBSUMED_BY) {
        val lit = Literal.makeLiteral(BooleanValue.TRUE)
        ExpressionTool.copyLocationInfo(this, lit)
        return lit
      } else if (relation == TypeHierarchy.DISJOINT) {
        if (!Cardinality.allowsZero(targetType.getCardinality) || !Cardinality.allowsZero(operand.getCardinality)) {
          val lit = Literal.makeLiteral(BooleanValue.FALSE)
          ExpressionTool.copyLocationInfo(this, lit)
          return lit
        }
      }
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
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    this
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = super.equals(other) && targetType == other

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = super.hashCode ^ targetType.hashCode

  /**
   * Determine the cardinality
   */
  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Determine the data type of the result of the InstanceOf expression
   */
  override def getItemType(): ItemType = AtomicType.BOOLEAN

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Evaluate the expression as a boolean
   */
  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val iter = operand.iterate(context)
    TypeChecker.testConformance(iter, targetType) == null
  }
}

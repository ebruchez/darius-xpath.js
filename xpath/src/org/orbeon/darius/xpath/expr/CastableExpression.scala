// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value.BooleanValue

import scala.util.control.Breaks

/**
 * Castable Expression: implements "Expr castable as atomic-type?".
 * The implementation simply wraps a cast expression with a try/catch.
 */
class CastableExpression(source: Expression, var targetType: AtomicType, var allowEmpty: Boolean)
    extends UnaryExpression(source) {

  /**
   * Simplify the expression
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    preEvaluate(visitor)
  }

  private def preEvaluate(visitor: ExpressionVisitor): Expression = {
    if (Literal.isAtomic(operand)) {
      return Literal.makeLiteral(BooleanValue.get(effectiveBooleanValue(new EarlyEvaluationContext(visitor.getConfiguration))))
    }
    if (Literal.isEmptySequence(operand)) {
      return new Literal(BooleanValue.get(allowEmpty))
    }
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    val th = TypeHierarchy.getInstance
    if (!CastExpression.isPossibleCast(operand.getItemType.getAtomizedItemType, targetType)) {
      return Literal.makeLiteral(BooleanValue.FALSE)
    }
    preEvaluate(visitor)
  }

  /**
   * Optimize the expression
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.optimize(operand, contextItemType)
    preEvaluate(visitor)
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    super.equals(other) &&
      targetType == other.asInstanceOf[CastableExpression].targetType && 
      allowEmpty == other.asInstanceOf[CastableExpression].allowEmpty
  }

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = super.hashCode ^ targetType.hashCode

  /**
   * Determine the data type of the result of the Castable expression
   */
  override def getItemType: ItemType = AtomicType.BOOLEAN

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Determine the special properties of this expression
   * @return [[StaticProperty.NON_CREATIVE]].
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    var count = 0
    val iter = operand.iterate(context)
    import Breaks._
    breakable {
      while (true) {
        val item = iter.next()
        if (item == null) {
          break()
        }
        val av = item.getTypedValue
        count += 1
        if (count > 1) {
          return false
        }
        //ORBEON Saxon was !!, not sure why
        if (av.convert(targetType).isInstanceOf[ValidationFailure]) {
          return false
        }
      }
    }
    count != 0 || allowEmpty
  }
}

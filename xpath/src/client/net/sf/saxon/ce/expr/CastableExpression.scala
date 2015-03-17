package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.BooleanValue
//remove if not needed
import scala.collection.JavaConversions._

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
  def simplify(visitor: ExpressionVisitor): Expression = {
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
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
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
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.optimize(operand, contextItemType)
    preEvaluate(visitor)
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    super == other && 
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
  def getItemType(): ItemType = AtomicType.BOOLEAN

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Determine the special properties of this expression
   * @return {@link StaticProperty#NON_CREATIVE}.
   */
  def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  def effectiveBooleanValue(context: XPathContext): Boolean = {
    var count = 0
    val iter = operand.iterate(context)
    while (true) {
      val item = iter.next()
      if (item == null) {
        //break
      }
      val av = item.getTypedValue
      count += 1
      if (count > 1) {
        return false
      }
      if (!!(av.convert(targetType).isInstanceOf[ValidationFailure])) {
        return false
      }
    }
    count != 0 || allowEmpty
  }
}

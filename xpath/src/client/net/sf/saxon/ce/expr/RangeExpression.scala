package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.SteppingIterator
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value._
import RangeExpression._
//remove if not needed
import scala.collection.JavaConversions._

object RangeExpression {

  /**
   * Function used by SteppingIterator to compute the next value in the sequence
   */
  private class RangeSteppingFunction(var limit: Int) extends SteppingIterator.SteppingFunction {

    def step(current: Item): Item = {
      val curr = current.asInstanceOf[IntegerValue].intValue()
      (if (curr >= limit) null else new IntegerValue(curr + 1))
    }

    def conforms(current: Item): Boolean = true
  }
}

/**
 * A RangeExpression is an expression that represents an integer sequence as
 * a pair of end-points (for example "x to y").
 * If the end-points are equal, the sequence is of length one.
 * <p>From Saxon 7.8, the sequence must be ascending; if the end-point is less
 * than the start-point, an empty sequence is returned. This is to allow
 * expressions of the form "for $i in 1 to count($seq) return ...." </p>
 */
class RangeExpression(start: Expression, op: Int, end: Expression) extends BinaryExpression(start, op, 
  end) {

  /**
   * Type-check the expression
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.typeCheck(operand0, contextItemType)
    operand1 = visitor.typeCheck(operand1, contextItemType)
    val backCompat = visitor.getStaticContext.isInBackwardsCompatibleMode
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, "to", 0)
    operand0 = TypeChecker.staticTypeCheck(operand0, SequenceType.OPTIONAL_INTEGER, backCompat, role0)
    val role1 = new RoleLocator(RoleLocator.BINARY_EXPR, "to", 1)
    operand1 = TypeChecker.staticTypeCheck(operand1, SequenceType.OPTIONAL_INTEGER, backCompat, role1)
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.optimize(operand0, contextItemType)
    operand1 = visitor.optimize(operand1, contextItemType)
    this
  }

  /**
   * Get the data type of the items returned
   */
  def getItemType(): ItemType = AtomicType.INTEGER

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  /**
   * Return an iteration over the sequence
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val av1 = operand0.evaluateItem(context).asInstanceOf[AtomicValue]
    if (av1 == null) {
      return EmptyIterator.getInstance
    }
    val v1 = av1.asInstanceOf[NumericValue]
    val av2 = operand1.evaluateItem(context).asInstanceOf[AtomicValue]
    if (av2 == null) {
      return EmptyIterator.getInstance
    }
    val v2 = av2.asInstanceOf[NumericValue]
    if (v1.compareTo(v2) > 0) {
      return EmptyIterator.getInstance
    }
    new SteppingIterator(v1, new RangeSteppingFunction(v2.intValue()), true)
  }
}

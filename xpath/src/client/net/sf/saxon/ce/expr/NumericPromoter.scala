package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A NumericPromoter performs numeric promotion on each item in a supplied sequence.
 * There are two subclasses, to handle promotion to double and promotion to float
 */
abstract class NumericPromoter(exp: Expression) extends UnaryExpression(exp) {

  /**
   * Simplify an expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    this
  }

  /**
   * Iterate over the sequence of values
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val base = operand.iterate(context)
    val promoter = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        return promote(item.asInstanceOf[AtomicValue])
      }
    }
    new ItemMappingIterator(base, promoter, true)
  }

  /**
   * Evaluate as an Item. This should only be called if the expression has cardinality zero-or-one
   */
  def evaluateItem(context: XPathContext): Item = {
    val item = operand.evaluateItem(context)
    if (item == null) return null
    promote(item.asInstanceOf[AtomicValue])
  }

  /**
   * Perform the promotion
   * @param value the numeric or untyped atomic value to be promoted
   * @return the value that results from the promotion
   */
  protected def promote(value: AtomicValue): AtomicValue
}

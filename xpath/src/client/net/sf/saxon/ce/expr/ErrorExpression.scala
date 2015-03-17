package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.ItemType
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Error expression: this expression is generated when the supplied expression cannot be
 * parsed, and the containing element enables forwards-compatible processing. It defers
 * the generation of an error message until an attempt is made to evaluate the expression
 */
class ErrorExpression(@BeanProperty var exception: XPathException) extends Expression {

  exception.setLocator(getSourceLocator)

  /**
   * Type-check the expression.
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  /**
   * Evaluate the expression. This always throws the exception registered when the expression
   * was first parsed.
   */
  def evaluateItem(context: XPathContext): Item = {
    val err = new XPathException(exception.getMessage)
    err.setLocator(getSourceLocator)
    err.setErrorCodeQName(exception.getErrorCodeQName)
    throw err
  }

  /**
   * Iterate over the expression. This always throws the exception registered when the expression
   * was first parsed.
   */
  def iterate(context: XPathContext): SequenceIterator = {
    evaluateItem(context)
    null
  }

  /**
   * Determine the data type of the expression, if possible
   * @return Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = AnyItemType.getInstance

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  private def copy(): Expression = new ErrorExpression(exception)
}

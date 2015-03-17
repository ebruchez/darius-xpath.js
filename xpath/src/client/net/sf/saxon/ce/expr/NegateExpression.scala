package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.NumericValue
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.DoubleValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Negate Expression: implements the unary minus operator.
 * This expression is initially created as an ArithmeticExpression (or in backwards
 * compatibility mode, an ArithmeticExpression10) to take advantage of the type checking code.
 * So we don't need to worry about type checking or argument conversion.
 */
class NegateExpression(base: Expression) extends UnaryExpression(base) {

  private var backwardsCompatible: Boolean = _

  /**
   * Set whether the expression is to be evaluated in XPath 1.0 compatibility mode
   * @param compatible true if XPath 1.0 compatibility mode is enabled
   */
  def setBackwardsCompatible(compatible: Boolean) {
    backwardsCompatible = compatible
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val oldop = operand
    val role = new RoleLocator(RoleLocator.UNARY_EXPR, "-", 0)
    operand = TypeChecker.staticTypeCheck(operand, SequenceType.OPTIONAL_NUMERIC, backwardsCompatible, 
      role)
    operand = visitor.typeCheck(operand, contextItemType)
    if (operand != oldop) {
      adoptChildExpression(operand)
    }
    this
  }

  /**
   * Determine the data type of the expression, if this is known statically
   */
  def getItemType(): ItemType = operand.getItemType

  /**
   * Evaluate the expression.
   */
  def evaluateItem(context: XPathContext): Item = {
    val v1 = operand.evaluateItem(context).asInstanceOf[NumericValue]
    if (v1 == null) {
      return if (backwardsCompatible) DoubleValue.NaN else null
    }
    v1.negate()
  }
}

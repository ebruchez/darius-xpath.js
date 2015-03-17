package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.value._
import client.net.sf.saxon.ce.value.StringValue
import NumberFn._
//remove if not needed
import scala.collection.JavaConversions._

object NumberFn {

  /**
   * Static method to perform the same conversion as the number() function. This is different from the
   * convert(Type.DOUBLE) in that it produces NaN rather than an error for non-numeric operands.
   * @param value the value to be converted
   * @return the result of the conversion
   */
  def convert(value: AtomicValue): DoubleValue = {
    try {
      if (value == null) {
        return DoubleValue.NaN
      }
      if (value.isInstanceOf[BooleanValue] || value.isInstanceOf[NumericValue]) {
        val result = value.convert(AtomicType.DOUBLE)
        if (result.isInstanceOf[ValidationFailure]) {
          return DoubleValue.NaN
        } else {
          return result.asInstanceOf[DoubleValue]
        }
      }
      if (value.isInstanceOf[StringValue] && !(value.isInstanceOf[AnyURIValue])) {
        val d = StringToDouble.stringToNumber(value.getStringValue)
        return new DoubleValue(d)
      }
      DoubleValue.NaN
    } catch {
      case e: NumberFormatException => DoubleValue.NaN
    }
  }
}

/**
 * Implements the XPath number() function.
 */
class NumberFn extends SystemFunction {

  def newInstance(): NumberFn = new NumberFn()

  /**
   * Simplify and validate.
   * This is a pure function so it can be simplified in advance if the arguments are known
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    argument(0).setFlattened(true)
    simplifyArguments(visitor)
  }

  /**
   * Type-check the expression. This also calls preEvaluate() to evaluate the function
   * if all the arguments are constant; functions that do not require this behavior
   * can override the preEvaluate method.
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e2 = super.typeCheck(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    if (argument(0).isInstanceOf[NumberFn]) {
      argument(0) = argument(0).asInstanceOf[NumberFn].argument(0)
    }
    this
  }

  /**
   * Evaluate in a general context
   */
  def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context)
    if (arg0 == null) {
      return DoubleValue.NaN
    }
    if (arg0.isInstanceOf[BooleanValue] || arg0.isInstanceOf[NumericValue]) {
      val result = arg0.asInstanceOf[AtomicValue].convert(AtomicType.DOUBLE)
      if (result.isInstanceOf[ValidationFailure]) {
        return DoubleValue.NaN
      } else {
        return result.asInstanceOf[AtomicValue]
      }
    }
    if (arg0.isInstanceOf[StringValue] && !(arg0.isInstanceOf[AnyURIValue])) {
      val s = arg0.getStringValue
      try {
        return new DoubleValue(StringToDouble.stringToNumber(s))
      } catch {
        case e: NumberFormatException => return DoubleValue.NaN
      }
    }
    DoubleValue.NaN
  }
}

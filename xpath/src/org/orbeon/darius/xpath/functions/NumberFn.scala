// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`._
import org.orbeon.darius.xpath.expr.{Expression, ExpressionVisitor, XPathContext}
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value.{StringValue, _}

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
      if (value.isInstanceOf[StringValue] && !value.isInstanceOf[AnyURIValue]) {
        val d = StringToDouble.stringToNumber(value.getStringValue)
        return new DoubleValue(d)
      }
      DoubleValue.NaN
    } catch {
      case e: NumberFormatException ⇒ DoubleValue.NaN
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
  override def simplify(visitor: ExpressionVisitor): Expression = {
    argument(0).setFlattened(true)
    simplifyArguments(visitor)
  }

  /**
   * Type-check the expression. This also calls preEvaluate() to evaluate the function
   * if all the arguments are constant; functions that do not require this behavior
   * can override the preEvaluate method.
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e2 = super.typeCheck(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    argument(0) match {
      case fn: NumberFn ⇒
        argument(0) = fn.argument(0)
      case _ ⇒
    }
    this
  }

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(context: XPathContext): Item = {
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
    if (arg0.isInstanceOf[StringValue] && !arg0.isInstanceOf[AnyURIValue]) {
      val s = arg0.getStringValue
      try {
        return new DoubleValue(StringToDouble.stringToNumber(s))
      } catch {
        case e: NumberFormatException ⇒ return DoubleValue.NaN
      }
    }
    DoubleValue.NaN
  }
}

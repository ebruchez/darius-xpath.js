// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType}
import client.net.sf.saxon.ce.functions.NumberFn
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value._

/**
 * Arithmetic Expression: an expression using one of the operators
 * plus, minus, multiply, div, idiv, mod, in backwards
 * compatibility mode: see [[ArithmeticExpression]] for the non-backwards
 * compatible case.
 */
class ArithmeticExpression10(p0: Expression, operator: Int, p1: Expression) extends BinaryExpression(p0, 
  operator, p1) {

  /**
   * Type-check the expression statically. We try to work out which particular
   * arithmetic function to use if the types of operands are known an compile time.
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e2 = super.typeCheck(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    val atomicType = SequenceType.OPTIONAL_ATOMIC
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 0)
    operand0 = TypeChecker.staticTypeCheck(operand0, atomicType, true, role0)
    val role1 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 1)
    operand1 = TypeChecker.staticTypeCheck(operand1, atomicType, true, role1)
    val itemType0 = operand0.getItemType
    if (itemType0.isInstanceOf[EmptySequenceTest]) {
      return Literal.makeLiteral(DoubleValue.NaN)
    }
    val itemType1 = operand1.getItemType
    if (itemType1.isInstanceOf[EmptySequenceTest]) {
      return Literal.makeLiteral(DoubleValue.NaN)
    }
    operand0 = createConversionCode(operand0)
    operand1 = createConversionCode(operand1)
    adoptChildExpression(operand0)
    adoptChildExpression(operand1)
    if (operator == Token.NEGATE) {
      operand1 match {
        case literal: Literal ⇒
          val v = literal.getValue
          v match {
            case value: NumericValue ⇒
              return Literal.makeLiteral(value.negate())
            case _ ⇒
          }
        case _ ⇒
      }
      val ne = new NegateExpression(operand1)
      ne.setBackwardsCompatible(true)
      return visitor.typeCheck(ne, contextItemType)
    }
    this
  }

  private def createConversionCode(_operand: Expression): Expression = {
    var operand = _operand
    if (Cardinality.allowsMany(operand.getCardinality)) {
      val fie = new FirstItemExpression(operand)
      ExpressionTool.copyLocationInfo(this, fie)
      operand = fie
    }
    operand
  }

  /**
   * Determine the data type of the expression, if this is known statically
   */
  def getItemType: ItemType = AtomicType.ANY_ATOMIC

  /**
   * Evaluate the expression.
   */
  override def evaluateItem(context: XPathContext): Item = {
    var v1 = operand0.evaluateItem(context).asInstanceOf[AtomicValue]
    v1 = convertOperand(v1)
    var v2 = operand1.evaluateItem(context).asInstanceOf[AtomicValue]
    v2 = convertOperand(v2)
    ArithmeticExpression.compute(v1, operator, v2, context)
  }

  private def convertOperand(value: AtomicValue): DoubleValue = {
    if (value == null) {
      return DoubleValue.NaN
    }
    value match {
      case value1: DoubleValue ⇒
        return value1
      case _ ⇒
    }
    val `type` = value.getItemType
    if (`type` == AtomicType.INTEGER || `type` == AtomicType.UNTYPED_ATOMIC || 
      `type` == AtomicType.DECIMAL || 
      `type` == AtomicType.FLOAT || 
      `type` == AtomicType.BOOLEAN || 
      `type` == AtomicType.STRING) {
      NumberFn.convert(value)
    } else {
      throw new XPathException("Invalid operand type for arithmetic: " + `type`, "XPTY0004")
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import java.math.BigDecimal

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr.ArithmeticExpression._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value._

object ArithmeticExpression {

  /**
   * Static method to apply arithmetic to two values
   *
   * @param _value0   the first value
   * @param operator the operator as denoted in the Token class, for example [[Token.PLUS]]
   * @param _value1   the second value
   * @param context  the XPath dynamic evaluation context
   * @return the result of the arithmetic operation
   */
  def compute(_value0: AtomicValue,
      operator: Int, 
      _value1: AtomicValue,
      context: XPathContext): AtomicValue = {

    var value0 = _value0
    var value1 = _value1

    var p0 = value0.getItemType
    var p1 = value1.getItemType
    val th = TypeHierarchy.getInstance
    if (p0 == AtomicType.UNTYPED_ATOMIC) {
      p0 = AtomicType.DOUBLE
      value0 = value0.convert(AtomicType.DOUBLE).asAtomic()
    }
    if (p1 == AtomicType.UNTYPED_ATOMIC) {
      p1 = AtomicType.DOUBLE
      value1 = value1.convert(AtomicType.DOUBLE).asAtomic()
    }
    if (p0 == AtomicType.DATE || p0 == AtomicType.TIME) {
      p0 = AtomicType.DATE_TIME
    }
    if (p1 == AtomicType.DATE || p1 == AtomicType.TIME) {
      p1 = AtomicType.DATE_TIME
    }
    if (value0.isInstanceOf[NumericValue] && value1.isInstanceOf[NumericValue]) {
      val n0 = value0.asInstanceOf[NumericValue]
      val n1 = value1.asInstanceOf[NumericValue]
      if (value0.isInstanceOf[DoubleValue] || value1.isInstanceOf[DoubleValue]) {
        val d0 = n0.getDoubleValue
        val d1 = n1.getDoubleValue
        var result: Double = 0.0
        operator match {
          case Token.PLUS  => result = d0 + d1
          case Token.MINUS => result = d0 - d1
          case Token.MULT => result = d0 * d1
          case Token.DIV => result = d0 / d1
          case Token.MOD => result = d0 % d1
          case Token.IDIV => 
            if (d1 == 0.0) {
              throw new XPathException("Integer division by zero", "FOAR0001")
            }
            if (d0.isNaN || d0.isInfinite) {
              throw new XPathException("First operand of idiv is NaN or infinity", "FOAR0002")
            }
            if (d1.isNaN) {
              throw new XPathException("Second operand of idiv is NaN", "FOAR0002")
            }
            return new DoubleValue(d0 / d1).convert(AtomicType.INTEGER)
              .asAtomic()

        }
        return new DoubleValue(result)
      } else if (value0.isInstanceOf[FloatValue] || value1.isInstanceOf[FloatValue]) {
        val f0 = n0.getFloatValue
        val f1 = n1.getFloatValue
        var result: Float = 0.0f
        operator match {
          case Token.PLUS  => result = f0 + f1
          case Token.MINUS => result = f0 - f1
          case Token.MULT => result = f0 * f1
          case Token.DIV => result = f0 / f1
          case Token.MOD => result = f0 % f1
          case Token.IDIV => 
            if (f1 == 0.0) {
              throw new XPathException("Integer division by zero", "FOAR0001")
            }
            if (f0.isNaN || f0.isInfinite) {
              throw new XPathException("First operand of idiv is NaN or infinity", "FOAR0002")
            }
            if (f1.isNaN) {
              throw new XPathException("Second operand of idiv is NaN", "FOAR0002")
            }
            return new FloatValue(f0 / f1).convert(AtomicType.INTEGER)
              .asAtomic()

        }
        return new FloatValue(result)
      } else {
        val d0 = n0.getDecimalValue
        val d1 = n1.getDecimalValue
        var result: BigDecimal = null
        operator match {
          case Token.PLUS  => result = d0.add(d1)
          case Token.MINUS => result = d0.subtract(d1)
          case Token.MULT => result = d0.multiply(d1)
          case Token.DIV => 
            var result1: BigDecimal = null
            val scale = Math.max(DecimalValue.DIVIDE_PRECISION, Math.max(d0.scale(), d1.scale()))
            try {
              result1 = d0.divide(d1, scale, BigDecimal.ROUND_HALF_DOWN)
            } catch {
              case err1: ArithmeticException => if (d1.signum() == 0) {
                throw new XPathException("Decimal divide by zero", "FOAR0001")
              } else {
                throw err1
              }
            }
            return new DecimalValue(result1)

          case Token.MOD => try {
            result = d0.remainder(d1)
          } catch {
            case err: ArithmeticException => if (n1.compareTo(0) == 0) {
              throw new XPathException("Decimal modulo zero", "FOAR0001")
            } else {
              throw err
            }
          }
          case Token.IDIV => 
            if (d1.signum() == 0) {
              throw new XPathException("Integer division by zero", "FOAR0001")
            }
            var quot = d0.divideToIntegralValue(d1)
            return IntegerValue.decimalToInteger(quot).asAtomic()

        }
        if (n0.isInstanceOf[IntegerValue] && n0.isInstanceOf[IntegerValue]) {
          return new IntegerValue(result)
        } else {
          return new DecimalValue(result)
        }
      }
    } else {
      if (p0 == AtomicType.DATE_TIME) {
        if (p1 == AtomicType.DATE_TIME && operator == Token.MINUS) {
          return value0.asInstanceOf[CalendarValue].subtract(value1.asInstanceOf[CalendarValue], context)
        } else if (th.isSubType(p1, AtomicType.DURATION) && 
          (operator == Token.PLUS || operator == Token.MINUS)) {
          var b = value1.asInstanceOf[DurationValue]
          if (operator == Token.MINUS) {
            b = b.multiply(-1.0)
          }
          return value0.asInstanceOf[CalendarValue].add(b)
        }
      } else if (th.isSubType(p0, AtomicType.DURATION)) {
        if (th.isSubType(p1, AtomicType.DURATION)) {
          val d0 = value1.asInstanceOf[DurationValue]
          val d1 = value1.asInstanceOf[DurationValue]
          operator match {
            case Token.PLUS => return d0.add(d1)
            case Token.MINUS => return d0.add(d1.negate())
            case Token.DIV => return d0.divide(d1)
          }
        } else if (p1 == AtomicType.DATE_TIME && operator == Token.PLUS) {
          return value1.asInstanceOf[CalendarValue].add(value0.asInstanceOf[DurationValue])
        } else if (th.isSubType(p1, AtomicType.NUMERIC) && (operator == Token.MULT || operator == Token.DIV)) {
          var d1 = value1.asInstanceOf[NumericValue].getDoubleValue
          if (operator == Token.DIV) {
            d1 = 1.0 / d1
          }
          return value0.asInstanceOf[DurationValue].multiply(d1)
        }
      } else if (th.isSubType(p0, AtomicType.NUMERIC) && th.isSubType(p1, AtomicType.DURATION) && 
        operator == Token.MULT) {
        return value1.asInstanceOf[DurationValue].multiply(value0.asInstanceOf[NumericValue].getDoubleValue)
      }
    }
    throw new XPathException("Undefined arithmetic operation: " + p0 + " " + Token.tokens(operator) + 
      " " + 
      p1, "XPTY0004")
  }
}

/**
 * Arithmetic Expression: an expression using one of the operators
 * plus, minus, multiply, div, idiv, mod. Note that this code does not handle backwards
 * compatibility mode: see [[ArithmeticExpression10]]
 */
class ArithmeticExpression(p0: Expression, operator: Int, p1: Expression) extends BinaryExpression(p0, 
  operator, p1) {

  protected var simplified: Boolean = false

  override def simplify(visitor: ExpressionVisitor): Expression = {
    if (simplified) {
      return this
    }
    simplified = true
    val e = super.simplify(visitor)
    if (e == this && 
      visitor.getStaticContext.isInBackwardsCompatibleMode) {
      new ArithmeticExpression10(operand0, operator, operand1)
    } else {
      if (operator == Token.NEGATE && Literal.isAtomic(operand1)) {
        val `val` = operand1.asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
        if (`val`.isInstanceOf[NumericValue]) {
          return new Literal(`val`.asInstanceOf[NumericValue].negate())
        }
      }
      e
    }
  }

  /**
   * Type-check the expression statically. We try to work out which particular
   * arithmetic function to use if the types of operands are known an compile time.
   */
  def NHtypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    val oldOp0 = operand0
    val oldOp1 = operand1
    operand0 = visitor.typeCheck(operand0, contextItemType)
    operand1 = visitor.typeCheck(operand1, contextItemType)
    val atomicType = SequenceType.OPTIONAL_ATOMIC
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 0)
    operand0 = TypeChecker.staticTypeCheck(operand0, atomicType, false, role0)
    val itemType0 = operand0.getItemType
    if (itemType0.isInstanceOf[EmptySequenceTest]) {
      return new Literal(EmptySequence.getInstance)
    }
    val type0 = itemType0.asInstanceOf[AtomicType]
    if (type0 == AtomicType.UNTYPED_ATOMIC) {
      operand0 = new UntypedAtomicConverter(operand0, AtomicType.DOUBLE, true, role0)
    } else if ((operand0.getSpecialProperties & StaticProperty.NOT_UNTYPED) == 
      0 && 
      th.relationship(type0, AtomicType.UNTYPED_ATOMIC) != TypeHierarchy.DISJOINT) {
      operand0 = new UntypedAtomicConverter(operand0, AtomicType.DOUBLE, false, role0)
    }
    val role1 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 1)
    operand1 = TypeChecker.staticTypeCheck(operand1, atomicType, false, role1)
    val itemType1 = operand1.getItemType
    if (itemType1.isInstanceOf[EmptySequenceTest]) {
      return new Literal(EmptySequence.getInstance)
    }
    val type1 = itemType1.asInstanceOf[AtomicType]
    if (type1 == AtomicType.UNTYPED_ATOMIC) {
      operand1 = new UntypedAtomicConverter(operand1, AtomicType.DOUBLE, true, role1)
    } else if ((operand1.getSpecialProperties & StaticProperty.NOT_UNTYPED) == 
      0 && 
      th.relationship(type1, AtomicType.UNTYPED_ATOMIC) != TypeHierarchy.DISJOINT) {
      operand1 = new UntypedAtomicConverter(operand1, AtomicType.DOUBLE, false, role1)
    }
    if (operand0 != oldOp0) {
      adoptChildExpression(operand0)
    }
    if (operand1 != oldOp1) {
      adoptChildExpression(operand1)
    }
    if (Literal.isEmptySequence(operand0) || Literal.isEmptySequence(operand1)) {
      return new Literal(EmptySequence.getInstance)
    }
    if (operator == Token.NEGATE) {
      if (operand1.isInstanceOf[Literal] && 
        operand1.asInstanceOf[Literal].getValue.isInstanceOf[NumericValue]) {
        val nv = operand1.asInstanceOf[Literal].getValue.asInstanceOf[NumericValue]
        return new Literal(nv.negate())
      } else {
        val ne = new NegateExpression(operand1)
        ne.setBackwardsCompatible(false)
        return visitor.typeCheck(ne, contextItemType)
      }
    }
    try {
      if (operand0.isInstanceOf[Literal] && operand1.isInstanceOf[Literal]) {
        return new Literal(evaluateItem(new EarlyEvaluationContext(visitor.getConfiguration)))
      }
    } catch {
      case err: XPathException => 
    }
    this
  }

  /**
   * Determine the data type of the expression, insofar as this is known statically
   *
   * @return the atomic type of the result of this arithmetic expression
   */
  def getItemType(): ItemType = {
    val t1 = operand0.getItemType.getAtomizedItemType
    val t2 = operand1.getItemType.getAtomizedItemType
    val th = TypeHierarchy.getInstance
    val numeric1 = th.isSubType(t1, AtomicType.NUMERIC)
    val numeric2 = th.isSubType(t2, AtomicType.NUMERIC)
    if (numeric1 && numeric2) {
      AtomicType.NUMERIC
    } else if ((numeric1 || numeric2) && 
      (operator == Token.PLUS || operator == Token.MINUS)) {
      AtomicType.NUMERIC
    } else {
      AtomicType.ANY_ATOMIC
    }
  }

  /**
   * Evaluate the expression.
   */
  override def evaluateItem(context: XPathContext): Item = {
    val v0 = operand0.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v0 == null) {
      return null
    }
    val v1 = operand1.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v1 == null) {
      return null
    }
    try {
      compute(v0, operator, v1, context)
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
  }
}

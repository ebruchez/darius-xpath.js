// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import java.math.BigDecimal

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, Type}
import client.net.sf.saxon.ce.expr.ValueComparison._
import client.net.sf.saxon.ce.expr.sort.{AtomicComparer, CodepointCollator, GenericAtomicComparer}
import client.net.sf.saxon.ce.functions.{Count, SystemFunction}
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value._

object ValueComparison {

  /**
   * Compare two atomic values, using a specified operator and collation
   *
   * @param v0       the first operand
   * @param op       the operator, as defined by constants such as [[client.net.sf.saxon.ce.expr.Token.FEQ]] or
   *                 [[client.net.sf.saxon.ce.expr.Token.FLT]]
   * @param v1       the second operand
   * @param comparer identifies the Collator to be used when comparing strings
   * @param checkTypes true if a check is required that the types of the arguments are comparable
   * @return the result of the comparison: -1 for LT, 0 for EQ, +1 for GT
   * @throws XPathException if the values are not comparable
   */
  def compare(v0: AtomicValue, 
      op: Int, 
      v1: AtomicValue, 
      comparer: AtomicComparer, 
      checkTypes: Boolean): Boolean = {
    if (checkTypes && 
      !Type.isComparable(v0.getItemType, v1.getItemType, Token.isOrderedOperator(op))) {
      throw new XPathException("Cannot compare " + Type.displayTypeName(v0) + " to " + 
        Type.displayTypeName(v1), "XPTY0004")
    }
    if (v0.isNaN || v1.isNaN) {
      return op == Token.FNE
    }
    try op match {
      case Token.FEQ ⇒ comparer.comparesEqual(v0, v1)
      case Token.FNE ⇒ !comparer.comparesEqual(v0, v1)
      case Token.FGT ⇒ comparer.compareAtomicValues(v0, v1) > 0
      case Token.FLT ⇒ comparer.compareAtomicValues(v0, v1) < 0
      case Token.FGE ⇒ comparer.compareAtomicValues(v0, v1) >= 0
      case Token.FLE ⇒ comparer.compareAtomicValues(v0, v1) <= 0
      case _ ⇒ throw new UnsupportedOperationException("Unknown operator " + op)
    } catch {
      case err: ClassCastException ⇒ {
        val e2 = new XPathException("Cannot compare " + Type.displayTypeName(v0) + " to " + 
          Type.displayTypeName(v1))
        e2.setErrorCode("XPTY0004")
        e2.setIsTypeError(true)
        throw e2
      }
    }
  }
}

/**
 * ValueComparison: a boolean expression that compares two atomic values
 * for equals, not-equals, greater-than or less-than. Implements the operators
 * eq, ne, lt, le, gt, ge
 */
class ValueComparison(p1: Expression, op: Int, p2: Expression) extends BinaryExpression(p1, op, p2) with ComparisonExpression {

  private var comparer: AtomicComparer = _

  private var needsRuntimeCheck: Boolean = _

  /**
   * Set the AtomicComparer used to compare atomic values
   * @param comparer the AtomicComparer
   */
  def setAtomicComparer(comparer: AtomicComparer): Unit = {
    this.comparer = comparer
  }

  /**
   * Get the AtomicComparer used to compare atomic values. This encapsulates any collation that is used.
   * Note that the comparer is always known at compile time.
   */
  def getAtomicComparer(): AtomicComparer = comparer

  /**
   * Get the primitive (singleton) operator used: one of Token.FEQ, Token.FNE, Token.FLT, Token.FGT,
   * Token.FLE, Token.FGE
   */
  def getSingletonOperator(): Int = operator

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val env = visitor.getStaticContext
    operand0 = visitor.typeCheck(operand0, contextItemType)
    if (Literal.isEmptySequence(operand0)) {
      return operand0
    }
    operand1 = visitor.typeCheck(operand1, contextItemType)
    if (Literal.isEmptySequence(operand1)) {
      return operand1
    }
    val optionalAtomic = SequenceType.OPTIONAL_ATOMIC
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 0)
    operand0 = TypeChecker.staticTypeCheck(operand0, optionalAtomic, false, role0)
    val role1 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 1)
    operand1 = TypeChecker.staticTypeCheck(operand1, optionalAtomic, false, role1)
    var p0 = operand0.getItemType.getAtomizedItemType
    var p1 = operand1.getItemType.getAtomizedItemType
    if (p0 == AtomicType.UNTYPED_ATOMIC) {
      p0 = AtomicType.STRING
    }
    if (p1 == AtomicType.UNTYPED_ATOMIC) {
      p1 = AtomicType.STRING
    }
    needsRuntimeCheck = p0 == AtomicType.ANY_ATOMIC || p1 == AtomicType.ANY_ATOMIC
    if (!needsRuntimeCheck && 
      !Type.isComparable(p0, p1, Token.isOrderedOperator(operator))) {
      val opt0 = Cardinality.allowsZero(operand0.getCardinality)
      val opt1 = Cardinality.allowsZero(operand1.getCardinality)
      if (opt0 || opt1) {
        needsRuntimeCheck = true
      } else {
        typeError("Cannot compare " + p0.toString + " to " + p1.toString, "XPTY0004")
      }
    }
    if (!(operator == Token.FEQ || operator == Token.FNE)) {
      if (!p0.isOrdered) {
        typeError("Type " + p0.toString + " is not an ordered type", "XPTY0004")
      }
      if (!p1.isOrdered) {
        typeError("Type " + p1.toString + " is not an ordered type", "XPTY0004")
      }
    }
    if (comparer == null) {
      val defaultCollationName = env.getDefaultCollationName
      var comp = env.getConfiguration.getNamedCollation(defaultCollationName)
      if (comp == null) {
        comp = CodepointCollator.getInstance
      }
      comparer = GenericAtomicComparer.makeAtomicComparer(p0, p1, comp, env.getConfiguration.getImplicitTimezone)
    }
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    if (operand0.isInstanceOf[Count] && Literal.isAtomic(operand1)) {
      val value1 = operand1.asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
      if (value1.isInstanceOf[NumericValue] && 
        value1.asInstanceOf[NumericValue].compareTo(0) == 0) {
        if (operator == Token.FEQ || operator == Token.FLE) {
          return SystemFunction.makeSystemFunction("empty", Array(operand0.asInstanceOf[FunctionCall].argument(0)))
        } else if (operator == Token.FNE || operator == Token.FGT) {
          return SystemFunction.makeSystemFunction("exists", Array(operand0.asInstanceOf[FunctionCall].argument(0)))
        } else if (operator == Token.FGE) {
          return Literal.makeLiteral(BooleanValue.TRUE)
        } else {
          return Literal.makeLiteral(BooleanValue.FALSE)
        }
      } else if (value1.isInstanceOf[IntegerValue] && (operator == Token.FGT || operator == Token.FGE)) {
        var `val` = value1.asInstanceOf[IntegerValue].intValue()
        if (operator == Token.FGT) {
          `val` += 1
        }
        val filter = new FilterExpression(operand0.asInstanceOf[FunctionCall].argument(0), Literal.makeLiteral(new IntegerValue(new BigDecimal(`val`))))
        ExpressionTool.copyLocationInfo(this, filter)
        return SystemFunction.makeSystemFunction("exists", Array(filter))
      }
    }
    if (operand1.isInstanceOf[Count] && Literal.isAtomic(operand0)) {
      val vc = new ValueComparison(operand1, Token.inverse(operator), operand0)
      ExpressionTool.copyLocationInfo(this, vc)
      return visitor.optimize(vc, contextItemType)
    }
    this
  }

  /**
   * Evaluate the effective boolean value of the expression
   *
   * @param context the given context for evaluation
   * @return a boolean representing the result of the comparison of the two operands
   */
  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    try {
      val v0 = operand0.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v0 == null) {
        return false
      }
      val v1 = operand1.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v1 == null) {
        return false
      }
      compare(v0, operator, v1, comparer, needsRuntimeCheck)
    } catch {
      case e: XPathException ⇒ {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
  }

  /**
   * Evaluate the expression in a given context
   *
   * @param context the given context for evaluation
   * @return a BooleanValue representing the result of the numeric comparison of the two operands,
   *         or null representing the empty sequence
   */
  override def evaluateItem(context: XPathContext): Item = {
    try {
      val v0 = operand0.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v0 == null) {
        return null
      }
      val v1 = operand1.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v1 == null) {
        return null
      }
      BooleanValue.get(compare(v0, operator, v1, comparer, needsRuntimeCheck))
    } catch {
      case e: XPathException ⇒ {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
  }

  /**
   * Determine the data type of the expression
   *
   * @return Type.BOOLEAN
   */
  def getItemType(): ItemType = AtomicType.BOOLEAN
}

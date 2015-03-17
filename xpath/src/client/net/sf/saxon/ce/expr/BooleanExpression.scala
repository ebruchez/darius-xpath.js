// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.functions.{BooleanFn, SystemFunction}
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.BooleanValue

/**
 * Boolean expression: two truth values combined using AND or OR.
 */
class BooleanExpression(p1: Expression, operator: Int, p2: Expression) extends BinaryExpression(p1, operator, 
  p2) {

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.typeCheck(visitor, contextItemType)
    if (e == this) {
      val err0 = TypeChecker.ebvError(operand0)
      if (err0 != null) {
        err0.setLocator(getSourceLocator)
        throw err0
      }
      val err1 = TypeChecker.ebvError(operand1)
      if (err1 != null) {
        err1.setLocator(getSourceLocator)
        throw err1
      }
      if (operand0.isInstanceOf[Literal] && 
        !(operand0.asInstanceOf[Literal].getValue.isInstanceOf[BooleanValue])) {
        operand0 = Literal.makeLiteral(BooleanValue.get(operand0.effectiveBooleanValue(null)))
      }
      if (operand1.isInstanceOf[Literal] && 
        !(operand1.asInstanceOf[Literal].getValue.isInstanceOf[BooleanValue])) {
        operand1 = Literal.makeLiteral(BooleanValue.get(operand1.effectiveBooleanValue(null)))
      }
    }
    e
  }

  /**
   * Determine the static cardinality. Returns [1..1]
   */
  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

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
    val th = TypeHierarchy.getInstance
    if (e != this) {
      return e
    }
    val op0 = BooleanFn.rewriteEffectiveBooleanValue(operand0, visitor, contextItemType)
    if (op0 != null) {
      operand0 = op0
    }
    val op1 = BooleanFn.rewriteEffectiveBooleanValue(operand1, visitor, contextItemType)
    if (op1 != null) {
      operand1 = op1
    }
    if (operator == Token.AND) {
      if (Literal.isConstantBoolean(operand0, false) || Literal.isConstantBoolean(operand1, false)) {
        return new Literal(BooleanValue.FALSE)
      } else if (Literal.isConstantBoolean(operand0, true)) {
        return forceToBoolean(operand1, th)
      } else if (Literal.isConstantBoolean(operand1, true)) {
        return forceToBoolean(operand0, th)
      }
    }
    if (operator == Token.OR) {
      if (Literal.isConstantBoolean(operand0, true) || Literal.isConstantBoolean(operand1, true)) {
        return new Literal(BooleanValue.TRUE)
      } else if (Literal.isConstantBoolean(operand0, false)) {
        return forceToBoolean(operand1, th)
      } else if (Literal.isConstantBoolean(operand1, false)) {
        return forceToBoolean(operand0, th)
      }
    }
//ORBEON unused UserFunctionCall
//    if (e == this && operator == Token.AND && operand1.isInstanceOf[UserFunctionCall] &&
//      th.isSubType(operand1.getItemType, AtomicType.BOOLEAN) &&
//      !visitor.isLoopingSubexpression(null)) {
//      val cond = Choose.makeConditional(operand0, operand1, Literal.makeLiteral(BooleanValue.FALSE))
//      ExpressionTool.copyLocationInfo(this, cond)
//      return cond
//    }
    this
  }

  private def forceToBoolean(in: Expression, th: TypeHierarchy): Expression = {
    if (in.getItemType == AtomicType.BOOLEAN && in.getCardinality == StaticProperty.ALLOWS_ONE) {
      in
    } else {
      SystemFunction.makeSystemFunction("boolean", Array(in))
    }
  }

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Evaluate as a boolean.
   */
  override def effectiveBooleanValue(c: XPathContext): Boolean = operator match {
    case Token.AND => operand0.effectiveBooleanValue(c) && operand1.effectiveBooleanValue(c)
    case Token.OR => operand0.effectiveBooleanValue(c) || operand1.effectiveBooleanValue(c)
    case _ => throw new UnsupportedOperationException("Unknown operator in boolean expression")
  }

  /**
   * Determine the data type of the expression
   * @return BuiltInAtomicType.BOOLEAN
   */
  def getItemType(): ItemType = AtomicType.BOOLEAN
}

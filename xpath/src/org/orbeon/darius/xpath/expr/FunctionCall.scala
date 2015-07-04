// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import java.{util ⇒ ju}

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr.FunctionCall._
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.value.SequenceExtent

import scala.collection.JavaConverters._

object FunctionCall {

  /**
   * Utility routine used in constructing error messages: get the word "argument" or "arguments"
   * @param num the number of arguments
   * @return the singular or plural word
   */
  private def pluralArguments(num: Int): String = {
    if (num == 1) return " argument"
    " arguments"
  }
}

/**
 * Abstract superclass for calls to system-defined and user-defined functions
 */
abstract class FunctionCall extends Expression {

  /**
   * The name of the function
   */
  private var name: StructuredQName = _

  /**
   * The array of expressions representing the actual parameters
   * to the function call
   */
  protected[expr] var argument: Array[Expression] = _

  /**
   * Set the name of the function being called
   * @param name the name of the function
   */
  def setFunctionName(name: StructuredQName): Unit = {
    this.name = name
  }

  /**
   * Get the qualified of the function being called
   * @return the qualified name
   */
  def getFunctionName: StructuredQName = name

  /**
   * Determine the number of actual arguments supplied in the function call
   * @return the arity (the number of arguments)
   */
  def getNumberOfArguments: Int = argument.length

  /**
   * Method called by the expression parser when all arguments have been supplied
   * @param args the expressions contained in the argument list of the function call
   */
  def setArguments(args: Array[Expression]): Unit = {
    argument = args
    for (a ← 0 until args.length) {
      adoptChildExpression(args(a))
    }
  }

  /**
   * Get the expressions supplied as actual arguments to the function
   * @return the array of expressions supplied in the argument list of the function call
   */
  def getArguments: Array[Expression] = argument

  /**
   * Simplify the function call. Default method is to simplify each of the supplied arguments and
   * evaluate the function if all are now known.
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = simplifyArguments(visitor)

  /**
   * Simplify the arguments of the function.
   * Called from the simplify() method of each function.
   * @return the result of simplifying the arguments of the expression
   * @param visitor an expression visitor
   */
  protected def simplifyArguments(visitor: ExpressionVisitor): Expression = {
    for (i ← 0 until argument.length) {
      val exp = visitor.simplify(argument(i))
      if (exp ne argument(i)) {
        adoptChildExpression(exp)
        argument(i) = exp
      }
    }
    this
  }

  /**
   * Type-check the expression. This also calls preEvaluate() to evaluate the function
   * if all the arguments are constant; functions that do not require this behavior
   * can override the preEvaluate method.
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    var fixed = true
    for (i ← 0 until argument.length) {
      val exp = visitor.typeCheck(argument(i), contextItemType)
      if (exp ne argument(i)) {
        adoptChildExpression(exp)
        argument(i) = exp
      }
      if (!argument(i).isInstanceOf[Literal]) {
        fixed = false
      }
    }
    checkArguments(visitor)
    if (fixed) {
      preEvaluate(visitor)
    } else {
      this
    }
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
   *                        [[org.orbeon.darius.xpath.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    var fixed = true
    for (i ← 0 until argument.length) {
      val exp = visitor.optimize(argument(i), contextItemType)
      if (exp ne argument(i)) {
        adoptChildExpression(exp)
        argument(i) = exp
      }
      if (fixed && !argument(i).isInstanceOf[Literal]) {
        fixed = false
      }
    }
    checkArguments(visitor)
    if (fixed) {
      preEvaluate(visitor)
    } else {
      this
    }
  }

  /**
   * Pre-evaluate a function at compile time. Functions that do not allow
   * pre-evaluation, or that need access to context information, can override this method.
   * @param visitor an expression visitor
   * @return the result of the early evaluation, or the original expression, or potentially
   * a simplified expression
   */
  def preEvaluate(visitor: ExpressionVisitor): Expression = {
    if (getIntrinsicDependencies != 0) {
      return this
    }
    val lit = Literal.makeLiteral(SequenceExtent.makeSequenceExtent(iterate(new EarlyEvaluationContext(visitor.getConfiguration))))
    ExpressionTool.copyLocationInfo(this, lit)
    lit
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      if (offer.action != PromotionOffer.UNORDERED) {
        for (i ← 0 until argument.length) {
          argument(i) = doPromotion(argument(i), offer)
        }
      }
      this
    }
  }

  /**
   * Method supplied by each class of function to check arguments during parsing, when all
   * the argument expressions have been read
   * @param visitor the expression visitor
   */
  protected def checkArguments(visitor: ExpressionVisitor): Unit

  /**
   * Check number of arguments. <BR>
   * A convenience routine for use in subclasses.
   * @param min the minimum number of arguments allowed
   * @param max the maximum number of arguments allowed
   * @param visitor an expression visitor
   * @return the actual number of arguments
   * @throws org.orbeon.darius.xpath.trans.XPathException if the number of arguments is out of range
   */
  protected def checkArgumentCount(min: Int, max: Int, visitor: ExpressionVisitor): Int = {
    val numArgs = argument.length
    if (min == max && numArgs != min) {
      throw new XPathException("Function " + getDisplayName + " must have " + min + pluralArguments(min),
        getSourceLocator)
    }
    if (numArgs < min) {
      throw new XPathException("Function " + getDisplayName + " must have at least " +
        min +
        pluralArguments(min), getSourceLocator)
    }
    if (numArgs > max) {
      throw new XPathException("Function " + getDisplayName + " must have no more than " +
        max +
        pluralArguments(max), getSourceLocator)
    }
    numArgs
  }

  /**
   * Get the immediate subexpressions of this expression
   */
  override def iterateSubExpressions(): ju.Iterator[Expression] = argument.iterator.asJava

  /**
   * Get the name of the function for display in messages
   * @return  the name of the function as a lexical QName
   */
  def getDisplayName: String = {
    val fName = getFunctionName
    if (fName == null) "(anonymous)" else fName.getDisplayName
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString: String = {
    val buff = new FastStringBuffer(FastStringBuffer.SMALL)
    buff.append(getDisplayName)
    val iter = iterateSubExpressions()
    var first = true
    while (iter.hasNext) {
      buff.append(if (first) "(" else ", ")
      buff.append(iter.next().toString)
      first = false
    }
    buff.append(if (first) "()" else ")")
    buff.toString
  }

  /**
   * Determine whether two expressions are equivalent
   */
  override def equals(o: Any): Boolean = {
    if (! o.isInstanceOf[FunctionCall]) {
      return false
    }
    val f = o.asInstanceOf[FunctionCall]
    if (getFunctionName != f.getFunctionName) {
      return false
    }
    if (getNumberOfArguments != f.getNumberOfArguments) {
      return false
    }
    for (i ← 0 until getNumberOfArguments if argument(i) != f.argument(i)) {
      return false
    }
    true
  }

  /**
   * Get hashCode in support of equals() method
   */
  override def hashCode(): Int = {
    var h = getFunctionName.hashCode
    for (i ← 0 until getNumberOfArguments) {
      h ^= argument(i).hashCode
    }
    h
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.orbeon.{Configuration, Executable, Stack}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.SourceLocator

import scala.beans.BeanProperty

object ExpressionVisitor {

  /**
   * Factory method: make an expression visitor
   * @param env the static context
   * @return the new expression visitor
   */
  def make(env: StaticContext, exec: Executable): ExpressionVisitor = {
    val visitor = new ExpressionVisitor()
    visitor.setStaticContext(env)
    visitor.setExecutable(exec)
    visitor.setConfiguration(env.getConfiguration)
    visitor
  }
}

/**
 *  The ExpressionVisitor supports the various phases of processing of an expression tree which require
 *  a recursive walk of the tree structure visiting each node in turn. In maintains a stack holding the
 *  ancestor nodes of the node currently being visited.
 */
class ExpressionVisitor {

  private val stack: Stack[Expression] = new Stack[Expression]()

  @BeanProperty
  var executable: Executable = _

  @BeanProperty
  var staticContext: StaticContext = _

  @BeanProperty
  var configuration: Configuration = _

  private def getLastLocator(): SourceLocator = {
    val stackSize = stack.size
    val expr = new Array[Expression](stackSize)
    stack.toArray(expr)
    val result: SourceLocator = null
    var i = stackSize - 1
    while (i > -1) {
      if (expr(i).sourceLocator != null) {
        return expr(i).sourceLocator
      }
      i -= 1
    }
    result
  }

  def getLocation(): String = {
    val sl = getLastLocator
    var message = ""
    if (sl != null) {
      message = sl.getLocation
      val pos = message.indexOf(" in ")
      if (pos > -1) {
        message = message.substring(0, pos)
      }
    }
    message
  }

  /**
   * Simplify an expression, via the ExpressionVisitor
   * @param exp the expression to be simplified
   * @return the simplified expression
   * @throws XPathException
   */
  def simplify(exp: Expression): Expression = {
    if (exp != null) {
      stack.push(exp)
      val exp2 = exp.simplify(this)
      if (exp2 != exp) {
        ExpressionTool.copyLocationInfo(exp, exp2)
      }
      stack.pop()
      exp2
    } else {
      null
    }
  }

  /**
   * Type check an expression, via the ExpressionVisitor
   * @param exp the expression to be typechecked
   * @param contextItemType the static type of the context item for this expression
   * @return the expression that results from type checking (this may be wrapped in expressions that
   * perform dynamic checking of the item type or cardinality, or that perform atomization or numeric
   * promotion)
   * @throws XPathException if static type checking fails, that is, if the expression cannot possibly
   * deliver a value of the required type
   */
  def typeCheck(exp: Expression, contextItemType: ItemType): Expression = {
    if (exp != null) {
      stack.push(exp)
      val exp2 = exp.typeCheck(this, contextItemType)
      if (exp2 != exp) {
        ExpressionTool.copyLocationInfo(exp, exp2)
      }
      stack.pop()
      exp2
    } else {
      null
    }
  }

  /**
   * Optimize an expression, via the ExpressionVisitor
   * @param exp the expression to be typechecked
   * @param contextItemType the static type of the context item for this expression
   * @return the rewritten expression
   * @throws XPathException
   */
  def optimize(exp: Expression, contextItemType: ItemType): Expression = {
    if (exp != null) {
      stack.push(exp)
      val exp2 = exp.optimize(this, contextItemType)
      if (exp2 != exp) {
        ExpressionTool.copyLocationInfo(exp, exp2)
      }
      stack.pop()
      exp2
    } else {
      null
    }
  }

  /**
   * Return true if the current expression at the top of the visitor's stack is evaluated repeatedly
   * when a given ancestor expression is evaluated once
   * @param ancestor the ancestor expression. May be null, in which case the search goes all the way
   * to the base of the stack.
   * @return true if the current expression is evaluated repeatedly
   */
  def isLoopingSubexpression(ancestor: Expression): Boolean = {
    var top = stack.size - 1
    while (true) {
      if (top <= 0) {
        return false
      }
      val parent = stack.get(top - 1)
      if (parent.hasLoopingSubexpression(stack.get(top))) {
        return true
      }
      if (parent == ancestor) {
        return false
      }
      top -= 1
    }
    false
  }

  /**
   * Reset the static properties for the current expression and for all its containing expressions.
   * This should be done whenever the expression is changed in a way that might
   * affect the properties. It causes the properties to be recomputed next time they are needed.
   */
  def resetStaticProperties() {
    val up = stack.iterator()
    while (up.hasNext) {
      val exp = up.next()
      exp.resetLocalStaticProperties()
    }
  }
}

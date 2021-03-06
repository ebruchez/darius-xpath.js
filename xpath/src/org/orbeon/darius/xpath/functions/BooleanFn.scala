// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.{AtomicType, ItemType, TypeHierarchy}
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.functions.BooleanFn._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.pattern.NodeTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.BooleanValue

object BooleanFn {

  val BOOLEAN = 0

  val NOT = 1

  /**
   * Optimize an expression whose effective boolean value is required
   * @param exp the expression whose EBV is to be evaluated
   * @param visitor an expression visitor
   * @param contextItemType the type of the context item for this expression
   * @return an expression that returns the EBV of exp, or null if no optimization was possible
   * @throws XPathException if static errors are found
   */
  def rewriteEffectiveBooleanValue(exp: Expression, visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    exp match {
      case fn: BooleanFn if fn.operation == BooleanFn.BOOLEAN ⇒
        fn.getArguments(0)
      case _ ⇒ if (th.isSubType(exp.getItemType, AtomicType.BOOLEAN) && exp.getCardinality == StaticProperty.EXACTLY_ONE) {
        exp
      } else exp match {
        case count: Count ⇒
          val exists = SystemFunction.makeSystemFunction("exists", count.getArguments)
          exists.setSourceLocator(exp.getSourceLocator)
          exists.optimize(visitor, contextItemType)
        case _ ⇒ if (exp.getItemType.isInstanceOf[NodeTest]) {
          val exists = SystemFunction.makeSystemFunction("exists", Array(exp))
          exists.setSourceLocator(exp.getSourceLocator)
          exists.optimize(visitor, contextItemType)
        } else {
          null
        }
      }
    }
  }
}

/**
 * This class supports the XPath functions boolean(), not(), true(), and false()
 */
class BooleanFn(_operation: Int) extends SystemFunction {

  this.operation = _operation

  def newInstance(): BooleanFn = new BooleanFn(operation)

  /**
   * Static analysis: prevent sorting of the argument
   */
  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    super.checkArguments(visitor)
    val err = TypeChecker.ebvError(argument(0))
    if (err != null) {
      err.setLocator(getSourceLocator)
      throw err
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
    val e = super.optimize(visitor, contextItemType)
    if (e == this) {
      if (operation == BOOLEAN) {
        val ebv = rewriteEffectiveBooleanValue(argument(0), visitor, contextItemType)
        return if (ebv == null) this else ebv.optimize(visitor, contextItemType)
      } else {
        val ebv = rewriteEffectiveBooleanValue(argument(0), visitor, contextItemType)
        if (ebv != null) {
          argument(0) = ebv
        }
        return this
      }
    }
    e
  }

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Evaluate the effective boolean value
   */
  override def effectiveBooleanValue(c: XPathContext): Boolean = {
    val b = argument(0).effectiveBooleanValue(c)
    if (operation == BOOLEAN) b else !b
  }
}

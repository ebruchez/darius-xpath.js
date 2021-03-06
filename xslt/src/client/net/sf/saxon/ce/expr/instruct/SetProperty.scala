// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.js.IXSLFunction
import org.orbeon.darius.xpath.pattern.EmptySequenceTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.ItemType
import com.google.gwt.core.client.JavaScriptObject
import java.util.Iterator
import SetProperty._
//remove if not needed
import scala.collection.JavaConversions._

object SetProperty {

  private def eval(ex: Expression, context: XPathContext): AnyRef = {
    IXSLFunction.convertToJavaScript(ExpressionTool.evaluate(ex, ExpressionTool.ITERATE_AND_MATERIALIZE, 
      context))
  }
}

class SetProperty(`object`: Expression, var select: Expression, var name: Expression)
    extends Instruction {

  private var targetObject: Expression = `object`

  adoptChildren()

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   * @param visitor an expression visitor
   * @return the simplified expression
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if an error is discovered during expression rewriting
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    targetObject = visitor.simplify(targetObject)
    name = visitor.simplify(name)
    select = visitor.simplify(select)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    targetObject = visitor.typeCheck(targetObject, contextItemType)
    name = visitor.typeCheck(name, contextItemType)
    select = visitor.typeCheck(select, contextItemType)
    adoptChildren()
    this
  }

  private def adoptChildren(): Unit = {
    adoptChildExpression(select)
    adoptChildExpression(targetObject)
    adoptChildExpression(name)
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    targetObject = visitor.optimize(targetObject, contextItemType)
    name = visitor.optimize(name, contextItemType)
    select = visitor.optimize(select, contextItemType)
    adoptChildren()
    this
  }

  def getIntrinsicDependencies: Int = StaticProperty.HAS_SIDE_EFFECTS

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws org.orbeon.darius.xpath.trans.XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    targetObject = doPromotion(targetObject, offer)
    name = doPromotion(name, offer)
    select = doPromotion(select, offer)
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   * @return the static item type of the instruction. This is empty: the set-attribute instruction
   *         returns nothing.
   */
  def getItemType: ItemType = EmptySequenceTest.getInstance

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    nonNullChildren(select, targetObject, name)
  }

  def processLeavingTail(context: XPathContext): TailCall = {
    val content = eval(select, context)
    val clientObject = eval(targetObject, context).asInstanceOf[JavaScriptObject]
    val member = eval(name, context).asInstanceOf[String]
    IXSLFunction.setProperty(clientObject, member, content)
    null
  }
}

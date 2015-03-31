// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import java.util.Iterator
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An abstract class to act as a common parent for instructions that create element nodes
 * and document nodes.
 */
abstract class ParentNodeConstructor extends Instruction {

  protected var content: Expression = _

  @BeanProperty
  var baseURI: String = _

  /**
   * Set the expression that constructs the content of the element
   * @param content the content expression
   */
  def setContentExpression(content: Expression): Unit = {
    this.content = content
    adoptChildExpression(content)
  }

  /**
   * Get the cardinality of the sequence returned by evaluating this instruction
   * @return the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   * @return the simplified expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during expression rewriting
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    content = visitor.simplify(content)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    content = visitor.typeCheck(content, contextItemType)
    adoptChildExpression(content)
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    content = visitor.optimize(content, contextItemType)
    if (content.isInstanceOf[Block]) {
      content = content.asInstanceOf[Block].mergeAdjacentTextInstructions()
    }
    adoptChildExpression(content)
    this
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    if (offer.action != PromotionOffer.UNORDERED) {
      content = doPromotion(content, offer)
    }
  }

  /**
   * Get the immediate sub-expressions of this expression.
   * @return an iterator containing the sub-expressions of this expression
   */
  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(content)

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true.
   */
  def createsNewNodes(): Boolean = true

  def getCardinality: Int = StaticProperty.EXACTLY_ONE
}

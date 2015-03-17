// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import java.util.Arrays
import java.util.Iterator
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Represents the set of xsl:param elements at the start of an xsl:iterate instruction
 */
class LocalParamBlock(params: Array[LocalParam]) extends Instruction {

  @BeanProperty
  var children: Array[LocalParam] = params

  for (c <- 0 until children.length) {
    adoptChildExpression(children(c))
  }

  def getExpressionName(): String = "block"

  def computeSpecialProperties(): Int = 0

  def iterateSubExpressions(): Iterator[Expression] = {
    Arrays.asList(children.asInstanceOf[Array[Expression]]:_*)
      .iterator()
  }

  /**
   * Determine the data type of the items returned by this expression
   * @return the data type
   */
  def getItemType(): ItemType = EmptySequenceTest.getInstance

  /**
   * Determine the cardinality of the expression
   */
  def getCardinality(): Int = StaticProperty.EMPTY

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   *
   * @exception client.net.sf.saxon.ce.trans.XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    for (c <- 0 until children.length) {
      children(c) = visitor.simplify(children(c)).asInstanceOf[LocalParam]
      adoptChildExpression(children(c))
    }
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (c <- 0 until children.length) {
      children(c) = visitor.typeCheck(children(c), contextItemType).asInstanceOf[LocalParam]
      adoptChildExpression(children(c))
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (c <- 0 until children.length) {
      children(c) = visitor.optimize(children(c), contextItemType).asInstanceOf[LocalParam]
      adoptChildExpression(children(c))
    }
    this
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  protected def promoteInst(offer: PromotionOffer) {
    for (c <- 0 until children.length) {
      children(c) = doPromotion(children(c), offer).asInstanceOf[LocalParam]
    }
  }

  def processLeavingTail(context: XPathContext): TailCall = {
    for (i <- 0 until children.length) {
      try {
        val param = children(i)
        context.setLocalVariable(param.getSlotNumber, param.getSelectValue(context))
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(children(i).getSourceLocator)
          throw e
        }
      }
    }
    null
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  def getImplementationMethod(): Int = PROCESS_METHOD
}

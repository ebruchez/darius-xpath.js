// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.lib.StandardErrorListener
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.ItemType
import java.util.Iterator
import java.util.logging.Level
import java.util.logging.Logger
import Message._
//remove if not needed
import scala.collection.JavaConversions._

object Message {

  private var logger: Logger = Logger.getLogger("Message")
}

/**
 * An xsl:message element in the stylesheet.
 */
class Message(var select: Expression, var terminate: Expression) extends Instruction {

  adoptChildExpression(terminate)

  adoptChildExpression(select)

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   * @return the simplified expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during expression rewriting
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    terminate = visitor.simplify(terminate)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.typeCheck(select, contextItemType)
    adoptChildExpression(select)
    if (terminate != null) {
      terminate = visitor.typeCheck(terminate, contextItemType)
      adoptChildExpression(terminate)
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.optimize(select, contextItemType)
    adoptChildExpression(select)
    if (terminate != null) {
      terminate = visitor.optimize(terminate, contextItemType)
      adoptChildExpression(terminate)
    }
    this
  }

  /**
   * Get the item type. To avoid spurious compile-time type errors, we falsely declare that the
   * instruction can return anything
   * @return AnyItemType
   */
  def getItemType: ItemType = AnyItemType.getInstance

  /**
   * Get the static cardinality. To avoid spurious compile-time type errors, we falsely declare that the
   * instruction returns zero or one items - this is always acceptable
   * @return zero or one
   */
  def getCardinality: Int = StaticProperty.ALLOWS_ZERO_OR_ONE

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true.
   */
  def createsNewNodes(): Boolean = true

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    if (select != null) {
      select = doPromotion(select, offer)
    }
    if (terminate != null) {
      terminate = doPromotion(terminate, offer)
    }
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select, terminate)

  def processLeavingTail(context: XPathContext): TailCall = {
    val iter = select.iterate(context)
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    var item = iter.next()
    if (item != null) {
      while (true) {
        sb.append(item.getStringValue)
        item = iter.next()
        if (item == null) {
          //break
        }
        sb.append(' ')
      }
    }
    val message = sb.toString
    var abort = false
    if (terminate != null) {
      val term = terminate.evaluateAsString(context).toString
      if (term == "no") {
      } else if (term == "yes") {
        abort = true
      } else {
        throw new XPathException("The terminate attribute of xsl:message must be 'yes' or 'no'", "XTDE0030")
      }
    }
    logger.log(Level.INFO, message)
    if (abort) {
      throw new TerminationException("Processing terminated by xsl:message in " + StandardErrorListener.abbreviatePath(getSystemId))
    }
    null
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.dom.HTMLNodeWrapper
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.trans.update.PendingUpdateList
import client.net.sf.saxon.ce.trans.update.SetAttributeAction
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import com.google.gwt.dom.client.Element
import java.util.Iterator
import SetAttribute._
//remove if not needed
import scala.collection.JavaConversions._

object SetAttribute {

  val SET = 0

  val REMOVE = 1
}

/**
 * The compiled form of an ixsl:set-attribute instruction in the stylesheet.
 */
class SetAttribute(var content: AttributeCreator, var action: Int) extends Instruction {

  adoptChildExpression(content)

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   * @param visitor an expression visitor
   * @return the simplified expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during expression rewriting
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    content = visitor.simplify(content)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    content = visitor.typeCheck(content, contextItemType)
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    content = visitor.optimize(content, contextItemType)
    this
  }

  def getIntrinsicDependencies(): Int = StaticProperty.HAS_SIDE_EFFECTS

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    content = doPromotion(content, offer)
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   * @return the static item type of the instruction. This is empty: the set-attribute instruction
   *         returns nothing.
   */
  def getItemType(): ItemType = EmptySequenceTest.getInstance

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(content)

  def processLeavingTail(context: XPathContext): TailCall = {
    val element = context.getContextItem
    if (!(element.isInstanceOf[HTMLNodeWrapper] && 
      element.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode.isInstanceOf[Element])) {
      return null
    }
    val parent = element.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode.asInstanceOf[Element]
    val pul = context.getController.getPendingUpdateList
    val att = content.evaluateItem(context)
    if (att.isInstanceOf[NodeInfo] && 
      att.asInstanceOf[NodeInfo].getNodeKind == Type.ATTRIBUTE) {
      pul.add(new SetAttributeAction(parent, att.asInstanceOf[NodeInfo].getURI, att.asInstanceOf[NodeInfo].getLocalPart, 
        (if (action == SET) att.getStringValue else null)))
    }
    null
  }
}

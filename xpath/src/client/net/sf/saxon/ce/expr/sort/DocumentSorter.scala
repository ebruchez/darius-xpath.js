// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.SequenceIterator

import scala.beans.BeanProperty

/**
 * A DocumentSorter is an expression that sorts a sequence of nodes into
 * document order.
 */
class DocumentSorter(base: Expression) extends UnaryExpression(base) {

  val props = base.getSpecialProperties

  @BeanProperty
  var comparer = if (((props & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0) ||
    (props & StaticProperty.SINGLE_DOCUMENT_NODESET) != 0) LocalOrderComparer.getInstance else GlobalOrderComparer.getInstance


  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    if ((operand.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
      0) {
      return operand
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.optimize(operand, contextItemType)
    if ((operand.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
      0) {
      return operand
    }
    this
  }

  override def computeSpecialProperties(): Int = {
    operand.getSpecialProperties | StaticProperty.ORDERED_NODESET
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      operand = doPromotion(operand, offer)
      this
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    new DocumentOrderIterator(operand.iterate(context), comparer)
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = operand.effectiveBooleanValue(context)
}

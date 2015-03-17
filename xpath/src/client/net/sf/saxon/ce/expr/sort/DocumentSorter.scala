package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A DocumentSorter is an expression that sorts a sequence of nodes into
 * document order.
 */
class DocumentSorter(base: Expression) extends UnaryExpression(base) {

  @BeanProperty
  var comparer: NodeOrderComparer = if (((props & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0) || 
    (props & StaticProperty.SINGLE_DOCUMENT_NODESET) != 0) LocalOrderComparer.getInstance else GlobalOrderComparer.getInstance

  val props = base.getSpecialProperties

  def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    if ((operand.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
      0) {
      return operand
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.optimize(operand, contextItemType)
    if ((operand.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
      0) {
      return operand
    }
    this
  }

  def computeSpecialProperties(): Int = {
    operand.getSpecialProperties | StaticProperty.ORDERED_NODESET
  }

  /**
   * Promote this expression if possible
   */
  def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      operand = doPromotion(operand, offer)
      this
    }
  }

  def iterate(context: XPathContext): SequenceIterator = {
    new DocumentOrderIterator(operand.iterate(context), comparer)
  }

  def effectiveBooleanValue(context: XPathContext): Boolean = operand.effectiveBooleanValue(context)
}

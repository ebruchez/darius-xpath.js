package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.IntegerValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implement the XPath 2.0 function last()
 */
class Last extends SystemFunction {

  def newInstance(): Last = new Last()

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (contextItemType == null) {
      dynamicError("The context for last() is undefined", "XPDY0002")
    }
    super.typeCheck(visitor, contextItemType)
  }

  /**
   * Promote this expression if possible
   */
  def promote(offer: PromotionOffer, parent: Expression): Expression = this

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = new IntegerValue(c.getLast)

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_LAST
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.IntegerValue

class Position extends SystemFunction {

  def newInstance(): Position = new Position()

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (contextItemType == null) {
      typeError("The context for position() is undefined", "XPDY0002")
    }
    super.typeCheck(visitor, contextItemType)
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = this

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = new IntegerValue(c.getContextPosition)

  /**
   * Determine the intrinsic dependencies
   */
  override def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_POSITION
}

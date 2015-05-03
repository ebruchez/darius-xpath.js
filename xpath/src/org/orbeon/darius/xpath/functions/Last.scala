// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value.IntegerValue

/**
 * Implement the XPath 2.0 function last()
 */
class Last extends SystemFunction {

  def newInstance(): Last = new Last()

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (contextItemType == null) {
      dynamicError("The context for last() is undefined", "XPDY0002")
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
  override def evaluateItem(c: XPathContext): Item = new IntegerValue(c.getLast)

  /**
   * Determine the dependencies
   */
  override def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_LAST
}

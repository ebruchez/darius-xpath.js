// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.tree.iter.UnfailingIterator

class UnfailingItemMappingIterator(base: UnfailingIterator, action: ItemMappingFunction)
    extends ItemMappingIterator(base, action) with UnfailingIterator {

  override def next(): Item = super.next()

  protected override def getBaseIterator: UnfailingIterator = {
    super.getBaseIterator.asInstanceOf[UnfailingIterator]
  }

  override def getAnother: UnfailingIterator = {
    val newBase = getBaseIterator.getAnother
    val action = getMappingFunction
    val newAction = action match {
      case function: StatefulMappingFunction ⇒ function.getAnother(newBase).asInstanceOf[ItemMappingFunction]
      case _ ⇒ action
    }
    new UnfailingItemMappingIterator(newBase, newAction)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.`type`

import org.orbeon.darius.xpath.om.Item

object AnyItemType {

  private val theInstance: AnyItemType = new AnyItemType()

  /**
   * Factory method to get the singleton instance
   */
  def getInstance: AnyItemType = theInstance
}

/**
 * An implementation of ItemType that matches any item (node or atomic value)
 */
class AnyItemType private () extends ItemType {

  /**
   * Test whether a given item conforms to this type
   *
   *
   * @param item The item to be tested
   * @return true if the item is an instance of this type; false otherwise
   */
  def matchesItem(item: Item): Boolean = true

  def getSuperType: ItemType = null

  def getAtomizedItemType: AtomicType = AtomicType.ANY_ATOMIC

  override def toString: String = "item()"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = "AnyItemType".hashCode
}

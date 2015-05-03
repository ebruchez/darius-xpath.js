// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.Sequence
import org.orbeon.darius.xpath.tree.iter.SingletonIterator
import org.orbeon.darius.xpath.tree.iter.UnfailingIterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A value that is a sequence containing zero or one items. Used only for items that are not atomic values
 * (that is, nodes, and function items)
 */
abstract class AbstractNode extends Item with Sequence {

  /**
   * Get the length of the sequence
   */
  def getLength: Int = 1

  /**
   * Get the n'th item in the sequence (starting from 0). This is defined for all
   * SequenceValues, but its real benefits come for a SequenceValue stored extensionally
   * (or for a MemoClosure, once all the values have been read)
   */
  def itemAt(n: Int): Item = if (n == 0) this else null

  /**
   * Return an enumeration of this nodeset value.
   */
  def iterate(): UnfailingIterator = SingletonIterator.makeIterator(this)
}

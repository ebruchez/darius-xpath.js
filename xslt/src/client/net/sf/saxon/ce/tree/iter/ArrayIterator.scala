// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.iter

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.Sequence
import org.orbeon.darius.xpath.value.SequenceExtent
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ArrayIterator is used to enumerate items held in an array.
 * The items are always held in the correct sorted order for the sequence.
 *
 * @author Michael H. Kay
 */
class ArrayIterator(protected var items: Array[Item]) extends UnfailingIterator with GroundedIterator {

  private var index: Int = 0

  /**
   * Get the next item in the array
   * @return the next item in the array
   */
  def next(): Item = {
    if (index >= items.length) {
      index = items.length + 1
      return null
    }
    items(index += 1)
  }

  /**
   * Get the number of items in the part of the array being processed
   *
   * @return the number of items; equivalently, the position of the last
   *     item
   */
  def getLastPosition: Int = items.length

  /**
   * Get another iterator over the same items
   *
   * @return a new ArrayIterator
   */
  def getAnother: UnfailingIterator = new ArrayIterator(items)

  /**
   * Return a SequenceValue containing all the items in the sequence returned by this
   * SequenceIterator
   *
   * @return the corresponding SequenceValue
   */
  def materialize(): Sequence = new SequenceExtent(items)
}
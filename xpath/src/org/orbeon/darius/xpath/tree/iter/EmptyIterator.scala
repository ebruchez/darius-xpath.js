// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.iter

import org.orbeon.darius.xpath.expr.LastPositionFinder
import org.orbeon.darius.xpath.om.{Item, Sequence}
import org.orbeon.darius.xpath.tree.iter.EmptyIterator._
import org.orbeon.darius.xpath.value.EmptySequence

object EmptyIterator {
  val getInstance = new EmptyIterator
}

/**
 * EmptyIterator: an iterator over an empty sequence. Since such an iterator has no state,
 * only one instance is required; therefore a singleton instance is available via the static
 * getInstance() method.
 */
class EmptyIterator private () extends UnfailingIterator with LastPositionFinder with GroundedIterator {

  /**
   * Get the next item.
   * @return the next item. For the EmptyIterator this is always null.
   */
  def next(): Item = null

  /**
   * Get the position of the last item in the sequence.
   * @return the position of the last item in the sequence, always zero in
   *     this implementation
   */
  def getLastPosition: Int = 0

  /**
   * Get another iterator over the same items, positioned at the start.
   * @return another iterator over an empty sequence (in practice, it
   *     returns the same iterator each time)
   */
  def getAnother: UnfailingIterator = getInstance

  /**
   * Return a Value containing all the items in the sequence returned by this
   * SequenceIterator. This should be an "in-memory" value, not a Closure.
   *
   * @return the corresponding Value
   */
  def materialize(): Sequence = EmptySequence.getInstance
}

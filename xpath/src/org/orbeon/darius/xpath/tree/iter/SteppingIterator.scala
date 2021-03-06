// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.iter

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.tree.iter.SteppingIterator._

object SteppingIterator {

  /**
   * Interface defining the function that steps from one item to the next.
   * Note that this function must not be stateful, and it must not throw
   * any errors.
   */
  trait SteppingFunction {

    /**
     * Step from one item to the next
     * @param current the current item
     * @return the next item, or null if there are no more items in the sequence
     */
    def step(current: Item): Item

    /**
     * Ask whether an item is to be included in the sequence, or skipped
     * @param current the item to be tested
     * @return true if the item is to be included in the sequence, false if it is to be skipped
     */
    def conforms(current: Item): Boolean
  }
}

/**
 * A general-purpose iterator built over a function that steps from one item in a sequence to the next
 */
class SteppingIterator(var origin: Item, var function: SteppingFunction, var includeSelf: Boolean)
    extends UnfailingIterator {

  private var _next: Item = origin

  if (!includeSelf || !function.conforms(origin)) {
    advance()
  }

  /**
   * Advance along the axis until a node is found that matches the required criteria
   */
  protected def advance(): Unit = {
    do {
      _next = function.step(_next)
    } while (_next != null && !function.conforms(_next))
  }

  /**
   * Return the next node in the iteration
   */
  def next(): Item = {
    if (_next == null) {
      null
    } else {
      val curr = _next
      advance()
      curr
    }
  }

  /**
   * Get another iterator over the same sequence of items, positioned at the
   * start of the sequence. It must be possible to call this method at any time, whether
   * none, some, or all of the items in the original iterator have been read. The method
   * is non-destructive: it does not change the state of the original iterator.
   * @return a new iterator over the same sequence
   */
  def getAnother: UnfailingIterator = {
    new SteppingIterator(origin, function, includeSelf)
  }
}

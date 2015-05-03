// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.iter

import org.orbeon.darius.xpath.expr.LastPositionFinder
import org.orbeon.darius.xpath.om.{Item, Sequence, SequenceIterator}

/**
 * This is an iterator over a sequence whose first item has already been read. On entry, the baseIterator
 * must be positioned so the second item in the sequence is the next item to be returned; the first item
 * in the sequence is available by calling current() on the baseIterator.
 *
 * <p>This avoids the cost of calling getAnother() to re-read the first item (which itself can be an
 * expensive operation, for example if it involves calling a user function).</p>
 */
class OneItemGoneIterator(var initial: Item, var baseIterator: SequenceIterator)
    extends SequenceIterator with LastPositionFinder with GroundedIterator {

  /**
   * Get the next item in the sequence. This method changes the state of the
   * iterator, in particular it affects the result of subsequent calls of
   * position() and current().
   * @return the next item, or null if there are no more items. Once a call
   *         on next() has returned null, no further calls should be made. The preferred
   *         action for an iterator if subsequent calls on next() are made is to return
   *         null again, and all implementations within Saxon follow this rule.
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if an error occurs retrieving the next item
   * @since 8.4
   */
  def next(): Item = {
    if (initial != null) {
      val first = initial
      initial = null
      first
    } else {
      baseIterator.next()
    }
  }

  /**
   * Get another SequenceIterator that iterates over the same items as the original,
   * but which is repositioned at the start of the sequence.
   * <p/>
   * This method allows access to all the items in the sequence without disturbing the
   * current position of the iterator. Internally, its main use is in evaluating the last()
   * function.
   * <p/>
   * This method does not change the state of the iterator.
   * @return a SequenceIterator that iterates over the same items,
   *         positioned before the first item
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if any error occurs
   * @since 8.4
   */
  def getAnother: SequenceIterator = baseIterator.getAnother

  /**
   * Get the last position (that is, the number of items in the sequence). This method is
   * non-destructive: it does not change the state of the iterator.
   * The result is undefined if the next() method of the iterator has already returned null.
   * This method returns -1 if the last position cannot be determined.
   */
  def getLastPosition: Int = {
    baseIterator match {
      case finder: LastPositionFinder ⇒ finder.getLastPosition
      case _ ⇒ -1
    }
  }

  /**
   * Return a GroundedValue containing all the items in the sequence returned by this
   * SequenceIterator. This should be an "in-memory" value, not a Closure.
   * @return the corresponding Value if the base iterator is grounded, or null otherwise.
   */
  def materialize(): Sequence = {
    baseIterator match {
      case iterator: GroundedIterator ⇒ iterator.materialize()
      case _ ⇒ null
    }
  }
}

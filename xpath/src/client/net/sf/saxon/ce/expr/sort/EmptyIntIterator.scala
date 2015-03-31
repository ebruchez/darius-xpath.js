// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr.z.IntIterator

object EmptyIntIterator {

  private val THE_INSTANCE: EmptyIntIterator = new EmptyIntIterator()

  /**
   * Get the singular instance of this class
   * @return the singular instance
   */
  def getInstance(): EmptyIntIterator = THE_INSTANCE
}

/**
 * An iterator over a zero-length sequence of integers
 */
class EmptyIntIterator private () extends IntIterator {

  /**
   * Test whether there are any more integers in the sequence
   *
   * @return true if there are more integers to come
   */
  def hasNext: Boolean = false

  /**
   * Return the next integer in the sequence. The result is undefined unless hasNext() has been called
   * and has returned true.
   *
   * @return the next integer in the sequence
   */
  def next(): Int = 0
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions.codenorm

/**
 *  Reimplementation of the JDK class BitSet, with heavily subsetted functionality.
 *  Handles a fixed-size bit set only; uses an int[] array rather than long[] as a concession
 *  to GWT.
 */
class BitSet(bits: Int) {

  private val words: Array[Int] = new Array[Int]((bits >> 5) + 1)

  /**
   * Set the n'th bit.
   * @param bit the bit to be set
   * @throws ArrayIndexOutOfBoundsException if n is negative or greater than the number of bits allocated
   * (rounded up to a multiple of 32)
   */
  def set(bit: Int): Unit = {
    val n = bit >> 5
    words(n) |= (1 << (bit & 0x1f))
  }

  /**
   * Get the value of the n'th bit
   * @param bit the bit to be read.
   * @return the value of the n'th bit, or false if n is outside the range of bits allocated.
   */
  def get(bit: Int): Boolean = {
    val n = bit >> 5
    if (n < 0 || n >= words.length) {
      return false
    }
    (words(n) & (1 << (bit & 0x1f))) != 0
  }
}

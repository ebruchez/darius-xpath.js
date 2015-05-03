// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.util

//remove if not needed
import scala.collection.JavaConversions._

object UTF16CharacterSet {

  val NONBMP_MIN = 0x10000

  val NONBMP_MAX = 0x10FFFF

  val SURROGATE1_MIN = 0xD800

  val SURROGATE1_MAX = 0xDBFF

  val SURROGATE2_MIN = 0xDC00

  val SURROGATE2_MAX = 0xDFFF

  /**
   * Return the non-BMP character corresponding to a given surrogate pair
   * surrogates.
   * @param high The high surrogate.
   * @param low The low surrogate.
   * @return the Unicode codepoint represented by the surrogate pair
   */
  def combinePair(high: Char, low: Char): Int = {
    (high - SURROGATE1_MIN) * 0x400 + (low - SURROGATE2_MIN) + 
      NONBMP_MIN
  }

  /**
   * Return the high surrogate of a non-BMP character
   * @param ch The Unicode codepoint of the non-BMP character to be divided.
   * @return the first character in the surrogate pair
   */
  def highSurrogate(ch: Int): Char = {
    (((ch - NONBMP_MIN) >> 10) + SURROGATE1_MIN).toChar
  }

  /**
   * Return the low surrogate of a non-BMP character
   * @param ch The Unicode codepoint of the non-BMP character to be divided.
   * @return the second character in the surrogate pair
   */
  def lowSurrogate(ch: Int): Char = {
    (((ch - NONBMP_MIN) & 0x3FF) + SURROGATE2_MIN).toChar
  }

  /**
   * Test whether a given character is a surrogate (high or low)
   * @param c the character to test
   * @return true if the character is the high or low half of a surrogate pair
   */
  def isSurrogate(c: Int): Boolean = (c & 0xF800) == 0xD800

  /**
   * Test whether the given character is a high surrogate
   * @param ch The character to test.
   * @return true if the character is the first character in a surrogate pair
   */
  def isHighSurrogate(ch: Int): Boolean = {
    SURROGATE1_MIN <= ch && ch <= SURROGATE1_MAX
  }

  /**
   * Test whether the given character is a low surrogate
   * @param ch The character to test.
   * @return true if the character is the second character in a surrogate pair
   */
  def isLowSurrogate(ch: Int): Boolean = {
    SURROGATE2_MIN <= ch && ch <= SURROGATE2_MAX
  }
}

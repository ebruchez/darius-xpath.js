// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.util

//remove if not needed
import scala.collection.JavaConversions._

object UTF8CharacterSet {

  /**
   * Static method to generate the UTF-8 representation of a Unicode character
   * @param in the Unicode character, or the high half of a surrogate pair
   * @param in2 the low half of a surrogate pair (ignored unless the first argument is in the
   * range for a surrogate pair)
   * @param out an array of at least 4 bytes to hold the UTF-8 representation.
   * @return the number of bytes in the UTF-8 representation
   */
  def getUTF8Encoding(in: Char, in2: Char, out: Array[Byte]): Int = {
    val i = in.toInt
    if (i <= 0x7f) {
      out(0) = i.toByte
      1
    } else if (i <= 0x7ff) {
      out(0) = (0xc0 | ((in >> 6) & 0x1f)).toByte
      out(1) = (0x80 | (in & 0x3f)).toByte
      2
    } else if (i >= 0xd800 && i <= 0xdbff) {
      val j = in2.toInt
      if (!(j >= 0xdc00 && j <= 0xdfff)) {
        throw new IllegalArgumentException("Malformed Unicode Surrogate Pair (" + i + ',' + j + ')')
      }
      val xxxxxx = (j & 0x3f).toByte
      val yyyyyy = (((i & 0x03) << 4) | ((j >> 6) & 0x0f)).toByte
      val zzzz = ((i >> 2) & 0x0f).toByte
      val uuuuu = (((i >> 6) & 0x0f) + 1).toByte
      out(0) = (0xf0 | ((uuuuu >> 2) & 0x07)).toByte
      out(1) = (0x80 | ((uuuuu & 0x03) << 4) | zzzz).toByte
      out(2) = (0x80 | yyyyyy).toByte
      out(3) = (0x80 | xxxxxx).toByte
      4
    } else if (i >= 0xdc00 && i <= 0xdfff) {
      0
    } else {
      out(0) = (0xe0 | ((in >> 12) & 0x0f)).toByte
      out(1) = (0x80 | ((in >> 6) & 0x3f)).toByte
      out(2) = (0x80 | (in & 0x3f)).toByte
      3
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.value._

object GeneralUnicodeString {

  def containsSurrogatePairs(value: CharSequence): Boolean = {
    for (i ← 0 until value.length) {
      val c = value.charAt(i).toInt
      if (c >= 55296 && c < 56319) {
        return true
      }
    }
    false
  }

  /**
   * Make a UnicodeString for a given CharSequence
   * @param in the input CharSequence
   * @return a UnicodeString using an appropriate implementation class
   */
  def makeUnicodeString(in: CharSequence): UnicodeString = {
    if (containsSurrogatePairs(in)) {
      GeneralUnicodeString(in)
    } else {
      new BMPString(in)
    }
  }

  def apply(in: CharSequence): GeneralUnicodeString = {
    val chars = StringValue.expand(in)
    new GeneralUnicodeString(chars, 0, chars.length)
  }
}

/**
 * A Unicode string which, in general, may contain non-BMP characters (that is, codepoints
 * outside the range 0-65535)
 */
class GeneralUnicodeString private (val chars: Array[Int], val start: Int, val end: Int) extends UnicodeString {

  def substring(beginIndex: Int, endIndex: Int): UnicodeString = {
    if (endIndex > chars.length) {
      throw new IndexOutOfBoundsException("endIndex=" + endIndex + "; sequence size=" + chars.length)
    }
    if (beginIndex < 0 || beginIndex > endIndex) {
      throw new IndexOutOfBoundsException("beginIndex=" + beginIndex + "; endIndex=" + endIndex)
    }
    new GeneralUnicodeString(chars, start + beginIndex, start + endIndex)
  }

  def charAt(pos: Int): Int = chars(start + pos)

  def indexOf(search: Int, pos: Int): Int = {
    (pos until length).find(x ⇒ chars(start + x) == search)
      .getOrElse(-1)
  }

  def length(): Int = end - start

  def isEnd(pos: Int): Boolean = pos >= (end - start)

  override def toString(): String = {
    var c = chars
    if (start != 0) {
      c = new Array[Int](end - start)
      System.arraycopy(chars, start, c, 0, end - start)
    }
    StringValue.contract(c, end - start).toString
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import java.util.Arrays

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.EscapeURI._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.tree.util.{FastStringBuffer, UTF16CharacterSet, UTF8CharacterSet}
import client.net.sf.saxon.ce.value.StringValue

object EscapeURI {

  val ENCODE_FOR_URI = 1

  val IRI_TO_URI = 2

  val HTML_URI = 3

  var allowedASCII: Array[Boolean] = new Array[Boolean](128)

  Arrays.fill(allowedASCII, 0, 32, false)

  Arrays.fill(allowedASCII, 33, 127, true)

  for (c <- Array('"', '<', '>', '\\', '^', '`', '{', '|', '}')) {
    allowedASCII(c) = false
  }

  /**
   * Escape special characters in a URI. The characters that are %HH-encoded are
   * all non-ASCII characters
   * @param s the URI to be escaped
   * @return the %HH-encoded string
   */
  def iriToUri(s: CharSequence): CharSequence = {
    if (allAllowedAscii(s)) {
      return s
    }
    val sb = new FastStringBuffer(s.length + 20)
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      if (c >= 0x7f || !allowedASCII(c.toInt)) {
        escapeChar(c, (if ((i + 1) < s.length) s.charAt(i + 1) else ' '), sb)
      } else {
        sb.append(c)
      }
    }
    sb
  }

  private def allAllowedAscii(s: CharSequence): Boolean = {
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      if (c >= 0x7f || !allowedASCII(c.toInt)) {
        return false
      }
    }
    true
  }

  /**
   * Escape special characters in a URI. The characters that are %HH-encoded are
   * all non-ASCII characters, plus all ASCII characters except (a) letter A-Z
   * and a-z, (b) digits 0-9, and (c) characters listed in the allowedPunctuation
   * argument
   * @param s the URI to be escaped
   * @param allowedPunctuation ASCII characters other than letters and digits that
   * should NOT be %HH-encoded
   * @return the %HH-encoded string
   */
  def escape(s: CharSequence, allowedPunctuation: String): CharSequence = {
    val sb = new FastStringBuffer(s.length)
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
        sb.append(c)
      } else if (c <= 0x20 || c >= 0x7f) {
        escapeChar(c, (if ((i + 1) < s.length) s.charAt(i + 1) else ' '), sb)
      } else if (allowedPunctuation.indexOf(c) >= 0) {
        sb.append(c)
      } else {
        escapeChar(c, ' ', sb)
      }
    }
    sb
  }

  private val hex = "0123456789ABCDEF"

  /**
   * Escape a single character in %HH representation, or a pair of two chars representing
   * a surrogate pair
   * @param c the character to be escaped, or the first character of a surrogate pair
   * @param c2 the second character of a surrogate pair
   * @param sb the buffer to contain the escaped result
   */
  private def escapeChar(c: Char, c2: Char, sb: FastStringBuffer) {
    val array = new Array[Byte](4)
    val used = UTF8CharacterSet.getUTF8Encoding(c, c2, array)
    for (b <- 0 until used) {
      val v = array(b).toInt & 0xff
      sb.append('%')
      sb.append(hex.charAt(v / 16))
      sb.append(hex.charAt(v % 16))
    }
  }

  /**
   * Escape a URI according to the HTML rules: that is, a non-ASCII character (specifically,
   * a character outside the range 32 - 126) is replaced by the %HH encoding of the octets in
   * its UTF-8 representation
   * @param url the URI to be escaped
   * @return the URI after escaping non-ASCII characters
   */
  def escapeHtmlURL(url: CharSequence): CharSequence = {
    val sb = new FastStringBuffer(url.length + 20)
    for (i <- 0 until url.length) {
      val ch = url.charAt(i)
      if (ch < 32 || ch > 126) {
        var c2 = ' '
        if (UTF16CharacterSet.isHighSurrogate(ch)) {
          c2 = url.charAt(i)
        }
        escapeChar(ch, c2, sb)
      } else {
        sb.append(ch)
      }
    }
    sb
  }
}

/**
 * This class supports the functions encode-for-uri() and iri-to-uri()
 */
class EscapeURI(_operation: Int) extends SystemFunction {

  this.operation = _operation

  def newInstance(): EscapeURI = new EscapeURI(operation)

  /**
   * Evaluate the function
   */
  override def evaluateItem(c: XPathContext): Item = {
    val item = argument(0).evaluateItem(c)
    if (item == null) {
      return StringValue.EMPTY_STRING
    }
    val s = item.getStringValue
    operation match {
      case ENCODE_FOR_URI => StringValue.makeStringValue(escape(s, "-_.~"))
      case IRI_TO_URI => StringValue.makeStringValue(iriToUri(s))
      case HTML_URI => StringValue.makeStringValue(escapeHtmlURL(s))
      case _ => throw new UnsupportedOperationException("Unknown escape operation")
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.number

import java.math.BigDecimal

import client.net.sf.saxon.ce.expr.number.NumberFormatter._
import client.net.sf.saxon.ce.lib.Numberer
import client.net.sf.saxon.ce.orbeon.{ArrayList, List}
import client.net.sf.saxon.ce.tree.util.{FastStringBuffer, UTF16CharacterSet}
import client.net.sf.saxon.ce.value.IntegerValue

import scala.util.control.Breaks

object NumberFormatter {

  /**
   * Determine whether a (possibly non-BMP) character is a letter or digit.
   * @param c the codepoint of the character to be tested
   * @return true if this is a number or letter as defined in the XSLT rules for xsl:number pictures.
   */
  private def isLetterOrDigit(c: Int): Boolean = {
    if (c <= 0x7F) {
      (c >= 0x30 && c <= 0x39) || (c >= 0x41 && c <= 0x5A) || 
        (c >= 0x61 && c <= 0x7A)
    } else {
      Alphanumeric.isAlphanumeric(c)
    }
  }
}

/**
 * Class NumberFormatter defines a method to format a ArrayList of integers as a character
 * string according to a supplied format specification.
 * @author Michael H. Kay
 */
class NumberFormatter {

  private var formatTokens: ArrayList[String] = _

  private var punctuationTokens: ArrayList[String] = _

  private var startsWithPunctuation: Boolean = _

  /**
   * Tokenize the format pattern.
   * @param _format the format specification. Contains one of the following values:<ul>
   * <li>"1": conventional decimal numbering</li>
   * <li>"a": sequence a, b, c, ... aa, ab, ac, ...</li>
   * <li>"A": sequence A, B, C, ... AA, AB, AC, ...</li>
   * <li>"i": sequence i, ii, iii, iv, v ...</li>
   * <li>"I": sequence I, II, III, IV, V, ...</li>
   * </ul>
   * This symbol may be preceded and followed by punctuation (any other characters) which is
   * copied to the output string.
   */
  def prepare(_format: String) {
    var format = _format
    if (format.length == 0) {
      format = "1"
    }
    formatTokens = new ArrayList[String](10)
    punctuationTokens = new ArrayList(10)
    val len = format.length
    var i = 0
    var t: Int = 0
    var first = true
    startsWithPunctuation = true
    import Breaks._
    breakable {
      while (i < len) {
        var c: Int = format.charAt(i)
        t = i
        if (UTF16CharacterSet.isHighSurrogate(c)) {
          c = UTF16CharacterSet.combinePair(c.toChar, format.charAt(i))
        }
        breakable {
          while (isLetterOrDigit(c)) {
            i += 1
            if (i == len)
              break()
            c = format.charAt(i)
            if (UTF16CharacterSet.isHighSurrogate(c)) {
              c = UTF16CharacterSet.combinePair(c.toChar, format.charAt(i))
            }
          }
        }
        if (i > t) {
          val tok = format.substring(t, i)
          formatTokens.add(tok)
          if (first) {
            punctuationTokens.add(".")
            startsWithPunctuation = false
            first = false
          }
        }
        if (i == len)
            break()
        t = i
        c = format.charAt(i)
        if (UTF16CharacterSet.isHighSurrogate(c)) {
          c = UTF16CharacterSet.combinePair(c.toChar, format.charAt(i))
        }
        breakable {
          while (!isLetterOrDigit(c)) {
            first = false
            i += 1
            if (i == len)
              break()
            c = format.charAt(i)
            if (UTF16CharacterSet.isHighSurrogate(c)) {
              c = UTF16CharacterSet.combinePair(c.toChar, format.charAt(i))
            }
          }
        }
        if (i > t) {
          val sep = format.substring(t, i)
          punctuationTokens.add(sep)
        }
      }
    }
    if (formatTokens.isEmpty) {
      formatTokens.add("1")
      if (punctuationTokens.size == 1) {
        punctuationTokens.add(punctuationTokens.get(0))
      }
    }
  }

  /**
   * Format a list of numbers.
   * @param numbers the numbers to be formatted (a sequence of integer values; it may also contain
   * preformatted strings as part of the error recovery fallback)
   * @return the formatted output string.
   */
  def format(numbers: List[_],
      groupSize: Int, 
      groupSeparator: String, 
      letterValue: String, 
      ordinal: String, 
      numberer: Numberer): CharSequence = {
    val sb = new FastStringBuffer(FastStringBuffer.TINY)
    var num = 0
    var tok = 0
    if (startsWithPunctuation) {
      sb.append(punctuationTokens.get(tok))
    }
    while (num < numbers.size) {
      if (num > 0) {
        if (tok == 0 && startsWithPunctuation) {
          sb.append(".")
        } else {
          sb.append(punctuationTokens.get(tok))
        }
      }
      val o = numbers.get(num)
      num += 1
      var s: String = null
      if (o.isInstanceOf[java.lang.Long]) {
        val nr = o.asInstanceOf[java.lang.Long].longValue()
        val rgf = new RegularGroupFormatter(groupSize, groupSeparator)
        s = numberer.format(nr, formatTokens.get(tok), rgf, letterValue, ordinal)
      } else s = if (o.isInstanceOf[BigDecimal]) new IntegerValue(o.asInstanceOf[BigDecimal]).getStringValue else o.toString
      sb.append(s)
      tok += 1
      if (tok == formatTokens.size) tok -= 1
    }
    if (punctuationTokens.size > formatTokens.size) {
      sb.append(punctuationTokens.get(punctuationTokens.size - 1))
    }
    sb.condense()
  }
}

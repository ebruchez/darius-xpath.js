// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import java.util.ArrayList
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

object Whitespace {

  /**
   * The values PRESERVE, REPLACE, and COLLAPSE represent the three options for whitespace
   * normalization. They are deliberately chosen in ascending strength order; given a number
   * of whitespace facets, only the strongest needs to be carried out.
   */
  val PRESERVE = 0

  val REPLACE = 1

  val COLLAPSE = 2

  /**
   * The values NONE, IGNORABLE, and ALL identify which kinds of whitespace text node
   * should be stripped when building a source tree. UNSPECIFIED indicates that no
   * particular request has been made. XSLT indicates that whitespace should be stripped
   * as defined by the xsl:strip-space and xsl:preserve-space declarations in the stylesheet
   */
  val NONE = 0

  val IGNORABLE = 1

  val ALL = 2

  val UNSPECIFIED = 3

  val XSLT = 4

  /**
   * Test whether a character is whitespace
   * @param ch the character (Unicode codepoint) to be tested
   * @return true if the character is one of tab, newline, carriage return, or space
   */
  def isWhitespace(ch: Int): Boolean = ch match {
    case 9 | 10 | 13 | 32 ⇒ true
    case _ ⇒ false
  }

  /**
   * Remove all whitespace characters from a string
   * @param value the string from which whitespace is to be removed
   * @return the string without its whitespace. This may be the original value
   * if it contained no whitespace
   */
  def removeAllWhitespace(value: CharSequence): CharSequence = {
    if (containsWhitespace(value)) {
      val sb = new FastStringBuffer(value.length)
      for (i ← 0 until value.length) {
        val c = value.charAt(i)
        if (c > 32 || !C0WHITE(c)) {
          sb.append(c)
        }
      }
      sb
    } else {
      value
    }
  }

  /**
   * Remove leading whitespace characters from a string
   * @param value the string whose leading whitespace is to be removed
   * @return the string with leading whitespace removed. This may be the
   * original string if there was no leading whitespace
   */
  def removeLeadingWhitespace(value: CharSequence): CharSequence = {
    var start = -1
    val len = value.length
    for (i ← 0 until len) {
      val c = value.charAt(i)
      if (c > 32 || !C0WHITE(c)) {
        start = i
        //break
      }
    }
    if (start == 0) {
      value
    } else if (start < 0 || start == len - 1) {
      ""
    } else {
      value.subSequence(start, len)
    }
  }

  /**
   * Determine if a string contains any whitespace
   * @param value the string to be tested
   * @return true if the string contains a character that is XML whitespace, that is
   * tab, newline, carriage return, or space
   */
  def containsWhitespace(value: CharSequence): Boolean = {
    val len = value.length
    var i = 0
    while (i < len) {
      val c = value.charAt(i += 1)
      if (c <= 32 && C0WHITE(c)) {
        return true
      }
    }
    false
  }

  /**
   * Determine if a string is all-whitespace
   *
   * @param content the string to be tested
   * @return true if the supplied string contains no non-whitespace
   *     characters
   */
  def isWhite(content: CharSequence): Boolean = {
    val len = content.length
    var i = 0
    while (i < len) {
      val c = content.charAt(i += 1)
      if (c > 32 || !C0WHITE(c)) {
        return false
      }
    }
    true
  }

  private var C0WHITE: Array[Boolean] = Array(false, false, false, false, false, false, false, false, false, true, true, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true)

  /**
   * Collapse whitespace as defined in XML Schema. This is equivalent to the
   * XPath normalize-space() function
   * @param in the string whose whitespace is to be collapsed
   * @return the string with any leading or trailing whitespace removed, and any
   * internal sequence of whitespace characters replaced with a single space character.
   */
  def collapseWhitespace(in: CharSequence): CharSequence = {
    val len = in.length
    if (len == 0 || !containsWhitespace(in)) {
      return in
    }
    val sb = new FastStringBuffer(len)
    var inWhitespace = true
    var i = 0
    while (i < len) {
      val c = in.charAt(i)
      c match {
        case '\n' | '\r' | '\t' | ' ' ⇒ if (inWhitespace) {
        } else {
          sb.append(' ')
          inWhitespace = true
        }
        case _ ⇒
          sb.append(c)
          inWhitespace = false

      }
      i += 1
    }
    val nlen = sb.length
    if (nlen > 0 && sb.charAt(nlen - 1) == ' ') {
      sb.setLength(nlen - 1)
    }
    sb
  }

  /**
   * Remove leading and trailing whitespace. This has the same effect as collapseWhitespace,
   * but is cheaper, for use by data types that do not allow internal whitespace.
   * @param in the input string whose whitespace is to be removed
   * @return the result of removing excess whitespace
   */
  def trimWhitespace(in: CharSequence): CharSequence = {
    if (in.length == 0) {
      return in
    }
    var first = 0
    var last = in.length - 1
    while (true) {
      val x = in.charAt(first)
      if (x > 32 || !C0WHITE(x)) {
        //break
      }
      if (first += 1 >= last) {
        return ""
      }
    }
    while (true) {
      val x = in.charAt(last)
      if (x > 32 || !C0WHITE(x)) {
        //break
      }
      last -= 1
    }
    if (first == 0 && last == in.length - 1) {
      in
    } else {
      in.subSequence(first, last + 1)
    }
  }

  /**
   * Trim leading and trailing whitespace from a string, returning a string.
   * This differs from the Java trim() method in that the only characters treated as
   * whitespace are space, \n, \r, and \t. The String#trim() method removes all C0
   * control characters (which is not the same thing under XML 1.1).
   * @param s the string to be trimmed. If null is supplied, null is returned.
   * @return the string with leading and trailing whitespace removed.
   */
  def trim(s: CharSequence): String = {
    if (s == null) {
      return null
    }
    trimWhitespace(s).toString
  }

  /**
   * Tokenize a string on whitespace boundaries
   * @param s the string to be tokenized
   * @return a list of tokens
   */
  def tokenize(s: CharSequence): List[String] = {
    val list = new ArrayList[String](8)
    val fsb = new FastStringBuffer(FastStringBuffer.TINY)
    var inTok = false
    for (i ← 0 until s.length) {
      val c = s.charAt(i)
      if (c <= 32 && C0WHITE(c)) {
        if (inTok) {
          list.add(fsb.toString)
          fsb.setLength(0)
          inTok = false
        }
      } else {
        fsb.append(c)
        inTok = true
      }
    }
    if (inTok) {
      list.add(fsb.toString)
    }
    list
  }
}

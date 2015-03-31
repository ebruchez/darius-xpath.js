// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.number

import client.net.sf.saxon.ce.expr.number.AbstractNumberer._
import client.net.sf.saxon.ce.lib.Numberer
import client.net.sf.saxon.ce.tree.util.{FastStringBuffer, UTF16CharacterSet}
import client.net.sf.saxon.ce.value.StringValue

import scala.beans.BeanProperty

object AbstractNumberer {

  val UPPER_CASE = 0
  val LOWER_CASE = 1
  val TITLE_CASE = 2

  protected val westernDigits = Array[Int]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  protected val latinUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  protected val latinLower = "abcdefghijklmnopqrstuvwxyz"

  protected val greekUpper = "ΑΒΓΔΕΖΗΘΙΚ" + "ΛΜΝΞΟΠΡ΢ΣΤ" + "ΥΦΧΨΩ"

  protected val greekLower = "αβγδεζηθικ" + "λμνξοπρςστ" + "υφχψω"

  /**
   * Generate a Roman numeral (in lower case)
   *
   * @param n the number to be formatted
   * @return the Roman numeral representation of the number in lower case
   */
  def toRoman(n: Long): String = {
    if (n <= 0 || n > 9999) {
      return "" + n
    }
    romanThousands(n.toInt / 1000) + romanHundreds((n.toInt / 100) % 10) + 
      romanTens((n.toInt / 10) % 10) + 
      romanUnits(n.toInt % 10)
  }

  private var romanThousands: Array[String] = Array("", "m", "mm", "mmm", "mmmm", "mmmmm", "mmmmmm", "mmmmmmm", "mmmmmmmm", "mmmmmmmmm")

  private var romanHundreds: Array[String] = Array("", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm")

  private var romanTens: Array[String] = Array("", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc")

  private var romanUnits: Array[String] = Array("", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix")
}

/**
 * Class AbstractNumberer is a base implementation of Numberer that provides language-independent
 * default numbering
 * This supports the xsl:number element.
 * Methods and data are declared as protected, and static is avoided, to allow easy subclassing.
 *
 * @author Michael H. Kay
 */
abstract class AbstractNumberer extends Numberer {

  @BeanProperty
  var country: String = _

  /**
   * Format a number into a string. This method is provided for backwards compatibility. It merely
   * calls the other format method after constructing a RegularGroupFormatter. The method is final;
   * localization subclasses should implement the method
   * [[format(long, String, NumericGroupFormatter, String, String)]] rather than this method.
   *
   * @param number         The number to be formatted
   * @param picture        The format token. This is a single component of the format attribute
   *                       of xsl:number, e.g. "1", "01", "i", or "a"
   * @param groupSize      number of digits per group (0 implies no grouping)
   * @param groupSeparator string to appear between groups of digits
   * @param letterValue    The letter-value specified to xsl:number: "alphabetic" or
   *                       "traditional". Can also be an empty string or null.
   * @param ordinal        The value of the ordinal attribute specified to xsl:number
   *                       The value "yes" indicates that ordinal numbers should be used; "" or null indicates
   *                       that cardinal numbers
   * @return the formatted number. Note that no errors are reported; if the request
   *         is invalid, the number is formatted as if the string() function were used.
   */
  def format(number: Long, 
      picture: String, 
      groupSize: Int, 
      groupSeparator: String, 
      letterValue: String, 
      ordinal: String): String = {
    format(number, picture, new RegularGroupFormatter(groupSize, groupSeparator), letterValue, ordinal)
  }

  /**
   * Format a number into a string
   *
   * @param number            The number to be formatted
   * @param picture           The format token. This is a single component of the format attribute
   *                          of xsl:number, e.g. "1", "01", "i", or "a"
   * @param numGroupFormatter object contains separators to appear between groups of digits
   * @param letterValue       The letter-value specified to xsl:number: "alphabetic" or
   *                          "traditional". Can also be an empty string or null.
   * @param ordinal           The value of the ordinal attribute specified to xsl:number
   *                          The value "yes" indicates that ordinal numbers should be used; "" or null indicates
   *                          that cardinal numbers
   * @return the formatted number. Note that no errors are reported; if the request
   *         is invalid, the number is formatted as if the string() function were used.
   */
  def format(number: Long, 
      picture: String, 
      numGroupFormatter: NumericGroupFormatter, 
      letterValue: String, 
      ordinal: String): String = {
    if (number < 0 || picture == null || picture.length == 0) {
      return "" + number
    }
    val pictureLength = StringValue.getStringLength(picture)
    val sb = new FastStringBuffer(FastStringBuffer.TINY)
    var formchar: Int = picture.charAt(0)
    if (UTF16CharacterSet.isHighSurrogate(formchar)) {
      formchar = UTF16CharacterSet.combinePair(formchar.toChar, picture.charAt(1))
    }
    formchar match {
      case '0' | '1' =>
        sb.append(toRadical(number, westernDigits, pictureLength, numGroupFormatter))
        if (ordinal != null && ordinal.length > 0) {
          sb.append(ordinalSuffix(ordinal, number))
        }

      case 'A' =>
        if (number == 0) {
          return "0"
        }
        return toAlphaSequence(number, latinUpper)

      case 'a' =>
        if (number == 0) {
          return "0"
        }
        return toAlphaSequence(number, latinLower)

      case 'w' | 'W' =>
        var wordCase: Int = 0
        wordCase = if (picture == "W") UPPER_CASE else if (picture == "w") LOWER_CASE else TITLE_CASE
        if (ordinal != null && ordinal.length > 0) {
          return toOrdinalWords(ordinal, number, wordCase)
        } else {
          return toWords(number, wordCase)
        }

      case 'i' => if (letterValue == null || letterValue.length == 0 || letterValue == "traditional") {
        return toRoman(number)
      }
      case 'I' => if (letterValue == null || letterValue.length == 0 || letterValue == "traditional") {
        return toRoman(number).toUpperCase()
      }
      case '①' =>
        if (number == 0 || number > 20) {
          return "" + number
        }
        return "" + (0x2460 + number - 1).toChar

      case '⑴' =>
        if (number == 0 || number > 20) {
          return "" + number
        }
        return "" + (0x2474 + number - 1).toChar

      case '⒈' =>
        if (number == 0 || number > 20) {
          return "" + number
        }
        return "" + (0x2488 + number - 1).toChar

      case 'Α' => return toAlphaSequence(number, greekUpper)
      case 'α' => return toAlphaSequence(number, greekLower)
      case _ =>
        var digitValue = Alphanumeric.getDigitValue(formchar)
        if (digitValue >= 0) {
          val zero = formchar - digitValue
          val digits = new Array[Int](10)
          var z = 0
          while (z <= 9) {
            digits(z) = zero + z
            z += 1
          }
          return toRadical(number, digits, pictureLength, numGroupFormatter)
        } else {
          if (number == 0) {
            return "0"
          }
          return toRadical(number, westernDigits, pictureLength, numGroupFormatter)
        }

    }
    sb.toString
  }

  /**
   * Construct the ordinal suffix for a number, for example "st", "nd", "rd". The default
   * (language-neutral) implementation returns a zero-length string
   *
   * @param ordinalParam the value of the ordinal attribute (used in non-English
   *                     language implementations)
   * @param number       the number being formatted
   * @return the ordinal suffix to be appended to the formatted number
   */
  protected def ordinalSuffix(ordinalParam: String, number: Long): String = ""

  /**
   * Format the number as an alphabetic label using the alphabet consisting
   * of consecutive Unicode characters from min to max
   *
   * @param number the number to be formatted
   * @param min    the start of the Unicode codepoint range
   * @param max    the end of the Unicode codepoint range
   * @return the formatted number
   */
  protected def toAlpha(number: Long, min: Int, max: Int): String = {
    if (number <= 0) {
      return "" + number
    }
    val range = max - min + 1
    val last = (((number - 1) % range) + min).toChar
    if (number > range) {
      toAlpha((number - 1) / range, min, max) + last
    } else {
      "" + last
    }
  }

  /**
   * Convert the number into an alphabetic label using a given alphabet.
   * For example, if the alphabet is "xyz" the sequence is x, y, z, xx, xy, xz, ....
   *
   * @param number   the number to be formatted
   * @param alphabet a string containing the characters to be used, for example "abc...xyz"
   * @return the formatted number
   */
  protected def toAlphaSequence(number: Long, alphabet: String): String = {
    if (number <= 0) {
      return "" + number
    }
    val range = alphabet.length
    val last = alphabet.charAt(((number - 1) % range).toInt)
    if (number > range) {
      toAlphaSequence((number - 1) / range, alphabet) + last
    } else {
      "" + last
    }
  }

  /**
   * Convert the number into a decimal or other representation using the given set of
   * digits.
   * For example, if the digits are "01" the sequence is 1, 10, 11, 100, 101, 110, 111, ...
   * More commonly, the digits will be "0123456789", giving the usual decimal numbering.
   *
   * @param number            the number to be formatted
   * @param digits            the codepoints to be used for the digits
   * @param pictureLength     the length of the picture that is significant: for example "3" if the
   *                          picture is "001"
   * @param numGroupFormatter an object that encapsulates the rules for inserting grouping separators
   * @return the formatted number
   */
  private def toRadical(number: Long, 
      digits: Array[Int], 
      pictureLength: Int, 
      numGroupFormatter: NumericGroupFormatter): String = {
    val temp = new FastStringBuffer(FastStringBuffer.TINY)
    val base = digits.length
    val s = new FastStringBuffer(FastStringBuffer.TINY)
    var n = number
    var count = 0
    while (n > 0) {
      val digit = digits((n % base).toInt)
      s.prependWideChar(digit)
      count += 1
      n = n / base
    }
    for (i <- 0 until (pictureLength - count)) {
      temp.appendWideChar(digits(0))
    }
    temp.append(s)
    if (numGroupFormatter == null) {
      return temp.toString
    }
    numGroupFormatter.format(temp)
  }

  /**
   * Show the number as words in title case. (We choose title case because
   * the result can then be converted algorithmically to lower case or upper case).
   *
   * @param number the number to be formatted
   * @return the number formatted as English words
   */
  def toWords(number: Long): String

  /**
   * Format a number as English words with specified case options
   *
   * @param number   the number to be formatted
   * @param wordCase the required case for example [[UPPER_CASE]],
   *                 [[LOWER_CASE]], [[TITLE_CASE]]
   * @return the formatted number
   */
  def toWords(number: Long, wordCase: Int): String = {
    var s: String = null
    s = if (number == 0) "Zero" else toWords(number)
    if (wordCase == UPPER_CASE) {
      s.toUpperCase()
    } else if (wordCase == LOWER_CASE) {
      s.toLowerCase()
    } else {
      s
    }
  }

  /**
   * Show an ordinal number as English words in a requested case (for example, Twentyfirst)
   *
   * @param ordinalParam the value of the "ordinal" attribute as supplied by the user
   * @param number       the number to be formatted
   * @param wordCase     the required case for example [[UPPER_CASE]],
   *                     [[LOWER_CASE]], [[TITLE_CASE]]
   * @return the formatted number
   */
  def toOrdinalWords(ordinalParam: String, number: Long, wordCase: Int): String

  /**
   * Get a month name or abbreviation
   *
   * @param month    The month number (1=January, 12=December)
   * @param minWidth The minimum number of characters
   * @param maxWidth The maximum number of characters
   */
  def monthName(month: Int, minWidth: Int, maxWidth: Int): String

  /**
   * Get a day name or abbreviation
   *
   * @param day      The day of the week (1=Monday, 7=Sunday)
   * @param minWidth The minimum number of characters
   * @param maxWidth The maximum number of characters
   */
  def dayName(day: Int, minWidth: Int, maxWidth: Int): String

  /**
   * Get an am/pm indicator. Default implementation works for English, on the basis that some
   * other languages might like to copy this (most non-English countries don't actually use the
   * 12-hour clock, so it's irrelevant).
   *
   * @param minutes  the minutes within the day
   * @param minWidth minimum width of output
   * @param maxWidth maximum width of output
   * @return the AM or PM indicator
   */
  def halfDayName(minutes: Int, minWidth: Int, maxWidth: Int): String = {
    var s: String = null
    if (minutes == 0 && maxWidth >= 8) {
      s = "Midnight"
    } else if (minutes < 12 * 60) maxWidth match {
      case 1 => s = "A"
      case 2 | 3 => s = "Am"
      case _ => s = "A.M."
    } else if (minutes == 12 * 60 && maxWidth >= 8) {
      s = "Noon"
    } else maxWidth match {
      case 1 => s = "P"
      case 2 | 3 => s = "Pm"
      case _ => s = "P.M."
    }
    s
  }

  /**
   * Get an ordinal suffix for a particular component of a date/time.
   *
   *
   * @param component the component specifier from a format-dateTime picture, for
   *                  example "M" for the month or "D" for the day.
   * @return a string that is acceptable in the ordinal attribute of xsl:number
   *         to achieve the required ordinal representation. For example, "-e" for the day component
   *         in German, to have the day represented as "dritte August".
   */
  def getOrdinalSuffixForDateTime(component: Char): String = "yes"

  /**
   * Get the name for an era (e.g. "BC" or "AD")
   *
   * @param year the proleptic gregorian year, using "0" for the year before 1AD
   */
  def getEraName(year: Int): String = (if (year > 0) "AD" else "BC")

  /**
   * Get the name of a calendar
   *
   * @param code The code representing the calendar as in the XSLT 2.0 spec, e.g. AD for the Gregorian calendar
   */
  def getCalendarName(code: String): String = {
    if (code == "AD") {
      "Gregorian"
    } else {
      code
    }
  }
}

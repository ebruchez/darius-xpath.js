package client.net.sf.saxon.ce.expr.number

import Numberer_en._
//remove if not needed
import scala.collection.JavaConversions._

object Numberer_en {

  private var englishUnits: Array[String] = Array("", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thir", "Four", "Fif", "Six", "Seven", "Eigh", "Nine")

  private var englishTens: Array[String] = Array("", "", "Twen", "Thir", "For", "Fif", "Six", "Seven", "Eigh", "Nine")

  private var englishOrdinalUnits: Array[String] = Array("", "First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth", "Eleventh", "Twelfth")

  private def abbreviate(name: String, max: Int): String = {
    (if (name.length > max) name.substring(0, 3) else name)
  }

  private var englishMonths: Array[String] = Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  private var englishDays: Array[String] = Array("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
}

/**
 * Numberer class for the English language.
 */
class Numberer_en extends AbstractNumberer {

  private var tensUnitsSeparatorCardinal: String = " "

  private var tensUnitsSeparatorOrdinal: String = "-"

  /**
   * Set the separator to be used between tens and units for cardinal numbers. This allows customization
   * of the output, for example "thirty six", "thirty-six", or "thirtysix". Default is a single space.
   *
   * @param separator the separator to be used between tens and units when cardinal numbers are written
   *                  as words.
   */
  def setTensUnitsSeparatorCardinal(separator: String) {
    tensUnitsSeparatorCardinal = separator
  }

  /**
   * Set the separator to be used between tens and units for ordinal numbers. This allows customization
   * of the output, for example "thirty sixth", "thirty-sixth", or "thirtysixth". Default is a hyphen.
   *
   * @param separator the separator to be used between tens and units when ordinal numbers are written
   *                  as words.
   */
  def setTensUnitsSeparatorOrdinal(separator: String) {
    tensUnitsSeparatorOrdinal = separator
  }

  /**
   * Construct the ordinal suffix for a number, for example "st", "nd", "rd"
   *
   * @param ordinalParam the value of the ordinal attribute (used in non-English
   *                     language implementations)
   * @param number       the number being formatted
   * @return the ordinal suffix to be appended to the formatted number
   */
  protected def ordinalSuffix(ordinalParam: String, number: Long): String = {
    val penult = (number % 100).toInt / 10
    val ult = (number % 10).toInt
    if (penult == 1) {
      "th"
    } else {
      if (ult == 1) {
        "st"
      } else if (ult == 2) {
        "nd"
      } else if (ult == 3) {
        "rd"
      } else {
        "th"
      }
    }
  }

  /**
   * Show the number as words in title case. (We choose title case because
   * the result can then be converted algorithmically to lower case or upper case).
   *
   * @param number the number to be formatted
   * @return the number formatted as English words
   */
  def toWords(number: Long): String = {
    if (number >= 1000000000) {
      val rem = number % 1000000000
      toWords(number / 1000000000) + " Billion" + 
        (if (rem == 0) "" else (if (rem < 100) " and " else " ") + toWords(rem))
    } else if (number >= 1000000) {
      val rem = number % 1000000
      toWords(number / 1000000) + " Million" + 
        (if (rem == 0) "" else (if (rem < 100) " and " else " ") + toWords(rem))
    } else if (number >= 1000) {
      val rem = number % 1000
      toWords(number / 1000) + " Thousand" + 
        (if (rem == 0) "" else (if (rem < 100) " and " else " ") + toWords(rem))
    } else if (number >= 100) {
      val rem = number % 100
      toWords(number / 100) + " Hundred" + (if (rem == 0) "" else " and " + toWords(rem))
    } else {
      if (number < 13) {
        return englishUnits(number.toInt)
      } else if (number < 20) {
        return englishUnits(number.toInt) + "teen"
      }
      val rem = (number % 10).toInt
      englishTens(number.toInt / 10) + "ty" + 
        (if (rem == 0) "" else tensUnitsSeparatorCardinal + englishUnits(rem))
    }
  }

  /**
   * Show an ordinal number as English words in a requested case (for example, Twentyfirst)
   *
   * @param ordinalParam the value of the "ordinal" attribute as supplied by the user
   * @param number       the number to be formatted
   * @param wordCase     the required case for example {@link #UPPER_CASE},
   *                     {@link #LOWER_CASE}, {@link #TITLE_CASE}
   * @return the formatted number
   */
  def toOrdinalWords(ordinalParam: String, number: Long, wordCase: Int): String = {
    var s: String = null
    if (number >= 1000000000) {
      val rem = number % 1000000000
      s = toWords(number / 1000000000) + " Billion" + 
        (if (rem == 0) "th" else (if (rem < 100) " and " else " ") + toOrdinalWords(ordinalParam, rem, 
        wordCase))
    } else if (number >= 1000000) {
      val rem = number % 1000000
      s = toWords(number / 1000000) + " Million" + 
        (if (rem == 0) "th" else (if (rem < 100) " and " else " ") + toOrdinalWords(ordinalParam, rem, 
        wordCase))
    } else if (number >= 1000) {
      val rem = number % 1000
      s = toWords(number / 1000) + " Thousand" + 
        (if (rem == 0) "th" else (if (rem < 100) " and " else " ") + toOrdinalWords(ordinalParam, rem, 
        wordCase))
    } else if (number >= 100) {
      val rem = number % 100
      s = toWords(number / 100) + " Hundred" + 
        (if (rem == 0) "th" else " and " + toOrdinalWords(ordinalParam, rem, wordCase))
    } else {
      if (number < 13) {
        s = englishOrdinalUnits(number.toInt)
      } else if (number < 20) {
        s = englishUnits(number.toInt) + "teenth"
      } else {
        val rem = (number % 10).toInt
        s = if (rem == 0) englishTens(number.toInt / 10) + "tieth" else englishTens(number.toInt / 10) + "ty" + tensUnitsSeparatorOrdinal + 
          englishOrdinalUnits(rem)
      }
    }
    if (wordCase == UPPER_CASE) {
      s.toUpperCase()
    } else if (wordCase == LOWER_CASE) {
      s.toLowerCase()
    } else {
      s
    }
  }

  /**
   * Get a month name or abbreviation
   *
   * @param month    The month number (1=January, 12=December)
   * @param minWidth The minimum number of characters
   * @param maxWidth The maximum number of characters
   */
  def monthName(month: Int, minWidth: Int, maxWidth: Int): String = {
    abbreviate(englishMonths(month - 1), maxWidth)
  }

  /**
   * Get a day name or abbreviation
   *
   * @param day      The day of the week (1=Monday, 7=Sunday)
   * @param minWidth The minimum number of characters
   * @param maxWidth The maximum number of characters
   */
  def dayName(day: Int, minWidth: Int, maxWidth: Int): String = {
    abbreviate(englishDays(day - 1), maxWidth)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import java.math.BigDecimal

import org.orbeon.darius.xpath.`type`.{AtomicType, ConversionResult, ValidationFailure}
import org.orbeon.darius.xpath.functions.Component
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.regex.ARegularExpression
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.FastStringBuffer

object DurationValue {

  /**
   * Static factory method: create a duration value from a supplied string, in
   * ISO 8601 format [-]PnYnMnDTnHnMnS
   *
   * @param s a string in the lexical space of xs:duration
   * @return the constructed xs:duration value, or a [[ValidationFailure]] if the
   *         supplied string is lexically invalid.
   */
  def makeDuration(s: CharSequence): ConversionResult = makeDuration(s, durationPattern1)

  private val durationPattern1 = ARegularExpression.make("-?P([0-9]+Y)?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?)?")

  private val durationPattern2 = ARegularExpression.make("[YMDHS]")

  def makeDuration(_s: CharSequence, constrainingPattern: ARegularExpression): ConversionResult = {
    val s = Whitespace.trimWhitespace(_s)
    if (!constrainingPattern.matches(s)) {
      badDuration("Incorrect format", s)
    }
    if (!durationPattern2.containsMatch(s)) {
      badDuration("No components present", s)
    }
    if (s.charAt(s.length - 1) == 'T') {
      badDuration("No component present after 'T'", s)
    }
    val negative = s.charAt(0) == '-'
    var inTimePart = false
    var positionOfDot = -1
    var year = 0
    var month = 0
    var day = 0
    var hour = 0
    var minute = 0
    var second = 0
    var micro = 0
    var part = 0
    for (i ← 0 until s.length) {
      val c = s.charAt(i)
      c match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒ part = part * 10 + (c - '0')
        case 'T' ⇒ inTimePart = true
        case 'Y' ⇒
          year = part
          part = 0

        case 'M' ⇒
          if (inTimePart) {
            minute = part
          } else {
            month = part
          }
          part = 0

        case 'D' ⇒
          day = part
          part = 0

        case 'H' ⇒
          hour = part
          part = 0

        case 'S' ⇒
          if (positionOfDot >= 0) {
            val fraction = (s.subSequence(positionOfDot + 1, i).toString + "000000")
              .substring(0, 6)
            micro = Integer.parseInt(fraction)
          } else {
            second = part
          }
          part = 0

        case '.' ⇒
          second = part
          part = 0
          positionOfDot = i

        case _ ⇒
      }
    }
    try {
      new DurationValue(!negative, year, month, day, hour, minute, second, micro)
    } catch {
      case err: IllegalArgumentException ⇒ new ValidationFailure(err.getMessage)
    }
  }

  protected def badDuration(msg: String, s: CharSequence): ValidationFailure = {
    new ValidationFailure("Invalid duration value '" + s + "' (" + msg + ')', "FORG0001")
  }

  /**
   * Parse a simple unsigned integer
   *
   * @param s the string containing the sequence of digits. No sign or whitespace is allowed.
   * @return the integer. Return -1 if the string is not a sequence of digits or exceeds 2^31
   */
  protected[value] def simpleInteger(s: String): Int = {
    var result = 0
    if (s == null) {
      return -1
    }
    val len = s.length
    if (len == 0) {
      return -1
    }
    for (i ← 0 until len) {
      val c = s.charAt(i)
      if (c >= '0' && c <= '9') {
        result = result * 10 + (c - '0')
        if (result > Integer.MAX_VALUE) {
          return -1
        }
      } else {
        return -1
      }
    }
    result
  }
}

/**
 * A value of type xs:duration
 */
class DurationValue protected () extends AtomicValue {

  protected var negative: Boolean = false

  protected var months: Int = 0

  protected var seconds: Long = 0

  protected var microseconds: Int = 0

  /**
   * Constructor for xs:duration taking the components of the duration. There is no requirement
   * that the values are normalized, for example it is acceptable to specify months=18. The values of
   * the individual components must all be non-negative.
   *
   * @param positive     true if the duration is positive, false if negative. For a negative duration
   *                     the components are all supplied as positive integers (or zero).
   * @param years        the number of years
   * @param months       the number of months
   * @param days         the number of days
   * @param hours        the number of hours
   * @param minutes      the number of minutes
   * @param seconds      the number of seconds
   * @param microseconds the number of microseconds
   * @throws IllegalArgumentException if the size of the duration exceeds implementation-defined
   * limits: specifically, if the total number of months exceeds 2^31, or if the total number
   * of seconds exceeds 2^63.
   */
  def this(positive: Boolean, 
      years: Int, 
      months: Int, 
      days: Int, 
      hours: Int, 
      minutes: Int, 
      seconds: Long, 
      microseconds: Int) {
    this()
    negative = !positive
    if (years < 0 || months < 0 || days < 0 || hours < 0 || minutes < 0 || 
      seconds < 0 || 
      microseconds < 0) {
      throw new IllegalArgumentException("Negative component value")
    }
    if (years.toDouble * 12 + months.toDouble > Integer.MAX_VALUE) {
      throw new IllegalArgumentException("Duration months limit exceeded")
    }
    if (days.toDouble * (24 * 60 * 60) + hours.toDouble * (60 * 60) + 
      minutes.toDouble * 60 + 
      seconds.toDouble > 
      Long.MaxValue) {
      throw new IllegalArgumentException("Duration seconds limit exceeded")
    }
    this.months = years * 12 + months
    val h = days * 24 + hours
    val m = h * 60 + minutes
    this.seconds = m * 60 + seconds
    this.microseconds = microseconds
    normalizeZeroDuration()
  }

  /**
   * Ensure that a zero duration is considered positive
   */
  protected def normalizeZeroDuration(): Unit = {
    if (months == 0 && seconds == 0L && microseconds == 0) {
      negative = false
    }
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType: AtomicType = AtomicType.DURATION

  /**
   * Convert to target data type
   *
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or a [[ValidationFailure]] if
   *         the value cannot be converted.
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.DURATION) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.YEAR_MONTH_DURATION) {
      YearMonthDurationValue.fromMonths(months * (if (negative) -1 else +1))
    } else if (requiredType == AtomicType.DAY_TIME_DURATION) {
      new DayTimeDurationValue(if (negative) -1 else +1, 0, 0, 0, seconds, microseconds)
    } else {
      new ValidationFailure("Cannot convert duration to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Ask whether the duration is negative (less than zero)
   * @return true if negative
   */
  def isNegative: Boolean = negative

  /**
   * Get the year component
   *
   * @return the number of years in the normalized duration; always positive
   */
  def getYears: Int = months / 12

  /**
   * Get the months component
   *
   * @return the number of months in the normalized duration; always positive
   */
  def getMonths: Int = months % 12

  /**
   * Get the days component
   *
   * @return the number of days in the normalized duration; always positive
   */
  def getDays: Int = (seconds / (24L * 60L * 60L)).toInt

  /**
   * Get the hours component
   *
   * @return the number of hours in the normalized duration; always positive
   */
  def getHours: Int = {
    (seconds % (24L * 60L * 60L) / (60L * 60L)).toInt
  }

  /**
   * Get the minutes component
   *
   * @return the number of minutes in the normalized duration; always positive
   */
  def getMinutes: Int = (seconds % (60L * 60L) / 60L).toInt

  /**
   * Get the seconds component
   *
   * @return the number of whole seconds in the normalized duration; always positive
   */
  def getSeconds: Int = (seconds % 60L).toInt

  /**
   * Get the microseconds component
   *
   * @return the number of microseconds in the normalized duration; always positive
   */
  def getMicroseconds: Int = microseconds

  /**
   * Convert to string
   *
   * @return ISO 8601 representation.
   */
  def getPrimitiveStringValue: CharSequence = {
    if (this.months == 0 && this.seconds == 0L && this.microseconds == 0) {
      return "PT0S"
    }
    val sb = new FastStringBuffer(32)
    if (negative) {
      sb.append('-')
    }
    val years = getYears
    val months = getMonths
    val days = getDays
    val hours = getHours
    val minutes = getMinutes
    val seconds = getSeconds
    sb.append("P")
    if (years != 0) {
      sb.append(years + "Y")
    }
    if (months != 0) {
      sb.append(months + "M")
    }
    if (days != 0) {
      sb.append(days + "D")
    }
    if (hours != 0 || minutes != 0 || seconds != 0 || microseconds != 0) {
      sb.append("T")
    }
    if (hours != 0) {
      sb.append(hours + "H")
    }
    if (minutes != 0) {
      sb.append(minutes + "M")
    }
    if (seconds != 0 || microseconds != 0) {
      if (seconds != 0 && microseconds == 0) {
        sb.append(seconds + "S")
      } else {
        val ms = (seconds * 1000000) + microseconds
        var mss = ms + ""
        if (seconds == 0) {
          mss = "0000000" + mss
          mss = mss.substring(mss.length - 7)
        }
        sb.append(mss.substring(0, mss.length - 6))
        sb.append('.')
        var lastSigDigit = mss.length - 1
        while (mss.charAt(lastSigDigit) == '0') {
          lastSigDigit -= 1
        }
        sb.append(mss.substring(mss.length - 6, lastSigDigit + 1))
        sb.append('S')
      }
    }
    sb
  }

  /**
   * Get length of duration in seconds, assuming an average length of month. (Note, this defines a total
   * ordering on durations which is different from the partial order defined in XML Schema; XPath 2.0
   * currently avoids defining an ordering at all. But the ordering here is consistent with the ordering
   * of the two duration subtypes in XPath 2.0.)
   *
   * @return the duration in seconds, as a double
   */
  def getLengthInSeconds: Double = {
    val a = months * (365.242199 / 12.0) * 24 * 60 * 60 + seconds + 
      (microseconds.toDouble / 1000000)
    if (negative) -a else a
  }

  /**
   * Get a component of the normalized value
   */
  override def getComponent(component: Int): AtomicValue = component match {
    case Component.YEAR ⇒
      var value5 = if (negative) -getYears else getYears
      new IntegerValue(value5)

    case Component.MONTH ⇒
      var value4 = if (negative) -getMonths else getMonths
      new IntegerValue(value4)

    case Component.DAY ⇒
      var value3 = if (negative) -getDays else getDays
      new IntegerValue(value3)

    case Component.HOURS ⇒
      var value2 = if (negative) -getHours else getHours
      new IntegerValue(value2)

    case Component.MINUTES ⇒
      var value1 = if (negative) -getMinutes else getMinutes
      new IntegerValue(value1)

    case Component.SECONDS ⇒
      var sb = new FastStringBuffer(FastStringBuffer.TINY)
      var ms = "000000" + microseconds
      ms = ms.substring(ms.length - 6)
      sb.append((if (negative) "-" else "") + getSeconds + '.' + ms)
      DecimalValue.makeDecimalValue(sb).asInstanceOf[AtomicValue]

    case Component.WHOLE_SECONDS ⇒ new IntegerValue(new BigDecimal(if (negative) -seconds else seconds))
    case Component.MICROSECONDS ⇒
      var value = if (negative) -microseconds else microseconds
      new IntegerValue(value)

    case _ ⇒ throw new IllegalArgumentException("Unknown component for duration: " + component)
  }

  /**
   * Get an object value that implements the XPath equality and ordering comparison semantics for this value.
   * If the ordered parameter is set to true, the result will be a Comparable and will support a compareTo()
   * method with the semantics of the XPath lt/gt operator, provided that the other operand is also obtained
   * using the getXPathComparable() method. In all cases the result will support equals() and hashCode() methods
   * that support the semantics of the XPath eq operator, again provided that the other operand is also obtained
   * using the getXPathComparable() method. A context argument is supplied for use in cases where the comparison
   * semantics are context-sensitive, for example where they depend on the implicit timezone or the default
   * collation.
   *
   * @param ordered true if an ordered comparison is required. In this case the result is null if the
   *                type is unordered; in other cases the returned value will be a Comparable.
   * @param collator not used when comparing durations
   * @param implicitTimezone not used when comparing durations
   */
  def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    if (ordered) null else this
  }

  /**
   * Test if the two durations are of equal length.
   *
   * @throws ClassCastException if the other value is not an xs:duration or subtype thereof
   */
  override def equals(other: Any): Boolean = other match {
    case other: DurationValue ⇒
      val d1 = this
      val d2 = other
      d1.negative == d2.negative && d1.months == d2.months &&
        d1.seconds == d2.seconds &&
        d1.microseconds == d2.microseconds
    case _ ⇒ false
  }

  override def hashCode(): Int = {
    new java.lang.Double(getLengthInSeconds).hashCode
  }

  /**
   * Add two durations
   *
   * @param other the duration to be added to this one
   * @return the sum of the two durations
   * @throws XPathException if, for example the durations are not both yearMonthDurations or dayTimeDurations
   */
  def add(other: DurationValue): DurationValue = {
    throw new XPathException("Only subtypes of xs:duration can be added", "XPTY0004")
  }

  /**
   * Negate a duration (same as subtracting from zero, but it preserves the type of the original duration)
   *
   * @return the original duration with its sign reversed, retaining its type
   */
  def negate(): DurationValue = {
    new DurationValue(negative, 0, months, 0, 0, 0, seconds, microseconds)
  }

  /**
   * Multiply a duration by a number
   *
   * @param factor the number to multiply by
   * @return the result of the multiplication
   * @throws XPathException for example if the type is wrong or overflow results
   */
  def multiply(factor: Double): DurationValue = {
    throw new XPathException("Only subtypes of xs:duration can be multiplied by a number", "XPTY0004")
  }

  /**
   * Divide a duration by a another duration
   *
   * @param other the duration to divide by
   * @return the result of the division
   * @throws XPathException for example if the type is wrong or overflow results
   */
  def divide(other: DurationValue): DecimalValue = {
    throw new XPathException("Only subtypes of xs:duration can be divided by another duration", "XPTY0004")
  }
}

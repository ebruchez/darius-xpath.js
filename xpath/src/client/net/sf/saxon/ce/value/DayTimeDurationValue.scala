// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import java.math.{BigDecimal, BigInteger}

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.orbeon.Util
import client.net.sf.saxon.ce.regex.ARegularExpression
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.DayTimeDurationValue._

object DayTimeDurationValue {

  val ONE_DAY = new DayTimeDurationValue(1, 1, 0, 0, 0, 0)

  val MINUS_ONE_DAY = new DayTimeDurationValue(-1, 1, 0, 0, 0, 0)

  private val DTDdurationPattern = ARegularExpression.make("-?P([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?)?")

  /**
   * Factory method: create a duration value from a supplied string, in
   * ISO 8601 format [-]PnDTnHnMnS
   *
   * @param s the lexical representation of the xs:dayTimeDuration value
   * @return a DayTimeDurationValue if the format is correct, or a ValidationErrorValue if not
   */
  def makeDayTimeDurationValue(s: CharSequence): ConversionResult = {
    val d = DurationValue.makeDuration(s, DTDdurationPattern)
    if (d.isInstanceOf[ValidationFailure]) {
      return d
    }
    val dv = d.asInstanceOf[DurationValue]
    dv.convert(AtomicType.DAY_TIME_DURATION)
  }

  /**
   * Construct a duration value as a number of seconds.
   *
   * @param _seconds the number of seconds in the duration. May be negative
   * @return the xs:dayTimeDuration value with the specified length
   */
  def fromSeconds(_seconds: BigDecimal): DayTimeDurationValue = {
    var seconds = _seconds
    val sdv = new DayTimeDurationValue()
    sdv.negative = seconds.signum() < 0
    if (sdv.negative) {
      seconds = seconds.negate()
    }
    val microseconds = seconds.multiply(DecimalValue.BIG_DECIMAL_ONE_MILLION)
    val intMicros = microseconds.toBigInteger()
    val parts = intMicros.divideAndRemainder(BigInteger.valueOf(1000000))
    sdv.seconds = parts(0).longValue()
    sdv.microseconds = parts(1).intValue()
    sdv
  }

  /**
   * Construct a duration value as a number of milliseconds.
   *
   * @param _milliseconds the number of milliseconds in the duration (may be negative)
   * @return the corresponding xs:dayTimeDuration value
   * @throws XPathException if implementation-defined limits are exceeded, specifically
   * if the total number of seconds exceeds 2^63.
   */
  def fromMilliseconds(_milliseconds: Long): DayTimeDurationValue = {
    var milliseconds = _milliseconds
    val sign = Util.signum(milliseconds)
    if (sign < 0) {
      milliseconds = -milliseconds
    }
    new DayTimeDurationValue(sign, 0, 0, 0, milliseconds / 1000, (milliseconds % 1000).toInt * 1000)
  }

  /**
   * Construct a duration value as a number of microseconds.
   *
   * @param _microseconds the number of microseconds in the duration. The maximum and minimum
   *                     limits are such that the number of days in the duration must fit in a 32-bit signed integer.
   * @return the xs:dayTimeDuration represented by the given number of microseconds
   * @throws IllegalArgumentException if the value is out of range.
   */
  def fromMicroseconds(_microseconds: Long): DayTimeDurationValue = {
    var microseconds = _microseconds
    val sign = Util.signum(microseconds)
    if (sign < 0) {
      microseconds = -microseconds
    }
    new DayTimeDurationValue(sign, 0, 0, 0, microseconds / 1000000, (microseconds % 1000000).toInt)
  }
}

/**
 * A value of type xs:dayTimeDuration
 */
class DayTimeDurationValue private () extends DurationValue with Comparable[AnyRef] {

  /**
   * Create a dayTimeDuration given the number of days, hours, minutes, and seconds. This
   * constructor performs no validation. The components (apart from sign) must all be non-negative
   * integers; they need not be normalized (for example, 36 hours is acceptable)
   *
   * @param sign         positive number for positive durations, negative for negative duratoins
   * @param days         number of days
   * @param hours        number of hours
   * @param minutes      number of minutes
   * @param seconds      number of seconds
   * @param _microseconds number of microseconds
   * @throws IllegalArgumentException if the value is out of range; specifically, if the total
   * number of seconds exceeds 2^63; or if any of the values is negative
   */
  def this(sign: Int, 
      days: Int, 
      hours: Int, 
      minutes: Int, 
      seconds: Long, 
      _microseconds: Int) {
    this()

    var microseconds = _microseconds

    if (days < 0 || hours < 0 || minutes < 0 || seconds < 0 || 
      microseconds < 0) {
      throw new IllegalArgumentException("Negative component value")
    }
    if (days.toDouble * (24 * 60 * 60) + hours.toDouble * (60 * 60) + 
      minutes.toDouble * 60 + 
      seconds.toDouble > 
      Long.MaxValue) {
      throw new IllegalArgumentException("Duration seconds limit exceeded")
    }
    negative = sign < 0
    months = 0
    val h = days.toLong * 24L + hours.toLong
    val m = h * 60L + minutes.toLong
    var s = m * 60L + seconds
    if (microseconds > 1000000) {
      s += microseconds / 1000000
      microseconds %= 1000000
    }
    this.seconds = s
    this.microseconds = microseconds
    if (s == 0 && microseconds == 0) {
      negative = false
    }
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  override def getItemType(): AtomicType = AtomicType.DAY_TIME_DURATION

  /**
   * Convert to string
   *
   * @return ISO 8601 representation.
   */
  override def getPrimitiveStringValue(): CharSequence = {
    val sb = new FastStringBuffer(32)
    if (negative) {
      sb.append('-')
    }
    val days = getDays
    val hours = getHours
    val minutes = getMinutes
    val seconds = getSeconds
    sb.append('P')
    if (days != 0) {
      sb.append(days + "D")
    }
    if (days == 0 || hours != 0 || minutes != 0 || seconds != 0 || 
      microseconds != 0) {
      sb.append('T')
    }
    if (hours != 0) {
      sb.append(hours + "H")
    }
    if (minutes != 0) {
      sb.append(minutes + "M")
    }
    if (seconds != 0 || microseconds != 0 || (days == 0 && minutes == 0 && hours == 0)) {
      if (microseconds == 0) {
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
   * Get length of duration in seconds
   */
  override def getLengthInSeconds(): Double = {
    val a = seconds + (microseconds.toDouble / 1000000)
    if (negative) -a else a
  }

  /**
   * Get length of duration in microseconds, as a long
   *
   * @return the length in microseconds
   */
  def getLengthInMicroseconds(): Long = {
    val a = seconds * 1000000 + microseconds
    if (negative) -a else a
  }

  /**
   * Multiply duration by a number. This is also used when dividing a duration by a number.
   */
  override def multiply(n: Double): DurationValue = {
    if (n.isNaN) {
      throw new XPathException("Cannot multiply/divide a duration by NaN", "FOCA0005")
    }
    val m = getLengthInMicroseconds.toDouble
    val product = n * m
    if (product.isInfinite || product.isNaN ||
      product > Long.MaxValue ||
      product < Long.MinValue) {
      throw new XPathException("Overflow when multiplying/dividing a duration by a number", "FODT0002")
    }
    try {
      fromMicroseconds(product.toLong)
    } catch {
      case err: IllegalArgumentException => if (err.getCause.isInstanceOf[XPathException]) {
        throw err.getCause.asInstanceOf[XPathException]
      } else {
        throw new XPathException("Overflow when multiplying/dividing a duration by a number", "FODT0002")
      }
    }
  }

  /**
   * Find the ratio between two durations
   *
   * @param other the dividend
   * @return the ratio, as a decimal
   * @throws XPathException
   */
  override def divide(other: DurationValue): DecimalValue = {
    if (other.isInstanceOf[DayTimeDurationValue]) {
      val v1 = BigDecimal.valueOf(getLengthInMicroseconds)
      val v2 = BigDecimal.valueOf(other.asInstanceOf[DayTimeDurationValue].getLengthInMicroseconds)
      if (v2.signum() == 0) {
        throw new XPathException("Divide by zero (durations)", "FOAR0001")
      }
      new DecimalValue(v1.divide(v2, 20, BigDecimal.ROUND_HALF_EVEN))
    } else {
      throw new XPathException("Cannot divide two durations of different type", "XPTY0004")
    }
  }

  /**
   * Add two dayTimeDurations
   */
  override def add(other: DurationValue): DurationValue = {
    if (other.isInstanceOf[DayTimeDurationValue]) {
      fromMicroseconds(getLengthInMicroseconds + 
        other.asInstanceOf[DayTimeDurationValue].getLengthInMicroseconds)
    } else {
      throw new XPathException("Cannot add two durations of different type", "XPTY0004")
    }
  }

  /**
   * Negate a duration (same as subtracting from zero, but it preserves the type of the original duration)
   *
   * @throws IllegalArgumentException in the extremely unlikely event that the duration is one that cannot
   *          be negated (because the limit for positive durations is one second
   *          off from the limit for negative durations)
   */
  override def negate(): DurationValue = {
    fromMicroseconds(-getLengthInMicroseconds)
  }

  /**
   * Compare the value to another duration value
   *
   * @param other The other dateTime value
   * @return negative value if this one is the earler, 0 if they are chronologically equal,
   *         positive value if this one is the later. For this purpose, dateTime values with an unknown
   *         timezone are considered to be UTC values (the Comparable interface requires
   *         a total ordering).
   * @throws ClassCastException if the other value is not a DateTimeValue (the parameter
   *                            is declared as Object to satisfy the Comparable interface)
   */
  def compareTo(other: AnyRef): Int = {
    if (other.isInstanceOf[DayTimeDurationValue]) {
      val diff = getLengthInMicroseconds - 
        other.asInstanceOf[DayTimeDurationValue].getLengthInMicroseconds
      if (diff < 0) {
        -1
      } else if (diff > 0) {
        +1
      } else {
        0
      }
    } else {
      throw new ClassCastException("Cannot compare a dayTimeDuration to an object of class " + 
        other.getClass)
    }
  }

  /**
   * Get a Comparable value that implements the XPath ordering comparison semantics for this value.
   * Returns null if the value is not comparable according to XPath rules. The default implementation
   * returns the value itself. This is modified for types such as
   * xs:duration which allow ordering comparisons in XML Schema, but not in XPath.
   * @param ordered true if an ordered comparable is needed
   * @param collator Collation used for string comparison
   * @param implicitTimezone
   */
  override def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    this
  }
}

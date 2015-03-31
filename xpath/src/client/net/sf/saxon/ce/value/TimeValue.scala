// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import java.math.BigDecimal

import client.net.sf.saxon.ce.`type`.{AtomicType, ConversionResult, ValidationFailure}
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.Component
import client.net.sf.saxon.ce.regex.RegExp
import client.net.sf.saxon.ce.trans.{Err, XPathException}
import client.net.sf.saxon.ce.tree.util.FastStringBuffer

import scala.beans.BeanProperty

object TimeValue {

  /**
   * Static factory method: create a time value from a supplied string, in
   * ISO 8601 format
   *
   * @param s the time in the lexical format hh:mm:ss[.ffffff] followed optionally by
   *          timezone in the form [+-]hh:mm or Z
   * @return either a TimeValue corresponding to the xs:time, or a ValidationFailure
   *         if the supplied value was invalid
   */
  private val timePattern: RegExp = RegExp.compile("([0-9][0-9]):([0-9][0-9]):([0-9][0-9])(\\.[0-9]*)?([-+Z].*)?")

  def makeTimeValue(s: CharSequence): ConversionResult = {
    val str = s.toString
    val `match` = timePattern.exec(str)
    if (`match` == null) {
      return badTime("wrong format", str)
    }
    val dt = new TimeValue()
    dt.hour = DurationValue.simpleInteger(`match`.getGroup(1))
    dt.minute = DurationValue.simpleInteger(`match`.getGroup(2))
    dt.second = DurationValue.simpleInteger(`match`.getGroup(3))
    val frac = `match`.getGroup(4)
    if (frac != null && frac.length > 0) {
      val fractionalSeconds = frac.toDouble
      dt.microsecond = Math.round(fractionalSeconds * 1000000).toInt
    }
    val tz = `match`.getGroup(5)
    val tzmin = CalendarValue.parseTimezone(tz)
    if (tzmin == CalendarValue.BAD_TIMEZONE) {
      return badTime("Invalid timezone", str)
    }
    dt.setTimezoneInMinutes(tzmin)
    if (dt.hour == 24) {
      if (dt.minute != 0 || dt.second != 0 || dt.microsecond != 0) {
        return badTime("after midnight", str)
      } else {
        dt.hour = 0
      }
    }
    dt
  }

  private def badTime(msg: String, value: CharSequence): ValidationFailure = {
    new ValidationFailure("Invalid time " + Err.wrap(value, Err.VALUE) + " (" + 
      msg + 
      ")", "FORG0001")
  }
}

/**
 * A value of type xs:time
 */
class TimeValue private () extends CalendarValue with Comparable[TimeValue] {

  @BeanProperty
  var hour: Int = _

  @BeanProperty
  var minute: Int = _

  @BeanProperty
  var second: Int = _

  @BeanProperty
  var microsecond: Int = _

  /**
   * Construct a time value given the hour, minute, second, and microsecond components.
   * This constructor performs no validation.
   *
   * @param hour        the hour value, 0-23
   * @param minute      the minutes value, 0-59
   * @param second      the seconds value, 0-59
   * @param microsecond the number of microseconds, 0-999999
   * @param tz          the timezone displacement in minutes from UTC. Supply the value
   *                    [[CalendarValue.NO_TIMEZONE]] if there is no timezone component.
   */
  def this(hour: Int, 
      minute: Int, 
      second: Int, 
      microsecond: Int, 
      tz: Int) {
    this()
    this.hour = hour
    this.minute = minute
    this.second = second
    this.microsecond = microsecond
    setTimezoneInMinutes(tz)
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.TIME

  /**
   * Convert to target data type
   *
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.TIME) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else {
      new ValidationFailure("Cannot convert gYear to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert to string
   *
   * @return ISO 8601 representation, in the localized timezone
   *         (the timezone held within the value).
   */
  def getPrimitiveStringValue(): CharSequence = {
    val sb = new FastStringBuffer(FastStringBuffer.TINY)
    CalendarValue.appendTwoDigits(sb, hour)
    sb.append(':')
    CalendarValue.appendTwoDigits(sb, minute)
    sb.append(':')
    CalendarValue.appendTwoDigits(sb, second)
    if (microsecond != 0) {
      sb.append('.')
      var ms = microsecond
      var div = 100000
      while (ms > 0) {
        val d = ms / div
        sb.append((d + '0').toChar)
        ms = ms % div
        div /= 10
      }
    }
    if (hasTimezone) {
      appendTimezone(sb)
    }
    sb
  }

  /**
   * Convert to a DateTime value. The date components represent a reference date, as defined
   * in the spec for comparing times.
   */
  def toDateTime(): DateTimeValue = {
    new DateTimeValue(1972, 12.toByte, 31.toByte, hour, minute, second, microsecond, getTimezoneInMinutes)
  }

  /**
   * Make a copy of this time value,
   * but with a different type label
   *
   */
  def copy(): AtomicValue = {
    new TimeValue(hour, minute, second, microsecond, getTimezoneInMinutes)
  }

  /**
   * Return a new time with the same normalized value, but
   * in a different timezone. This is called only for a TimeValue that has an explicit timezone
   *
   * @param timezone the new timezone offset, in minutes
   * @return the time in the new timezone. This will be a new TimeValue unless no change
   *         was required to the original value
   */
  def adjustTimezone(timezone: Int): CalendarValue = {
    val dt = toDateTime().adjustTimezone(timezone).asInstanceOf[DateTimeValue]
    new TimeValue(dt.getHour, dt.getMinute, dt.getSecond, dt.getMicrosecond, dt.getTimezoneInMinutes)
  }

  /**
   * Get a component of the value. Returns null if the timezone component is
   * requested and is not present.
   */
  override def getComponent(component: Int): AtomicValue = component match {
    case Component.HOURS ⇒ new IntegerValue(hour)
    case Component.MINUTES ⇒ new IntegerValue(minute)
    case Component.SECONDS ⇒
      var d = BigDecimal.valueOf(microsecond)
      d = d.divide(DecimalValue.BIG_DECIMAL_ONE_MILLION, 6, BigDecimal.ROUND_HALF_UP)
      d = d.add(BigDecimal.valueOf(second))
      new DecimalValue(d)

    case Component.WHOLE_SECONDS ⇒ new IntegerValue(second)
    case Component.MICROSECONDS ⇒ new IntegerValue(microsecond)
    case Component.TIMEZONE ⇒ if (hasTimezone) {
      DayTimeDurationValue.fromMilliseconds(60000L * getTimezoneInMinutes)
    } else {
      null
    }
    case _ ⇒ throw new IllegalArgumentException("Unknown component for time: " + component)
  }

  /**
   * Compare the value to another dateTime value
   *
   * @param other The other dateTime value
   * @return negative value if this one is the earler, 0 if they are chronologically equal,
   *         positive value if this one is the later. For this purpose, dateTime values with an unknown
   *         timezone are considered to be UTC values (the Comparable interface requires
   *         a total ordering).
   * @throws ClassCastException if the other value is not a TimeValue (the parameter
   *                            is declared as Object to satisfy the Comparable interface)
   */
  def compareTo(other: TimeValue): Int = {
    toDateTime().compareTo(other.toDateTime())
  }

  /**
   * Compare the value to another dateTime value
   *
   * @param other The other dateTime value
   * @param implicitTimezone
   * @return negative value if this one is the earler, 0 if they are chronologically equal,
   *         positive value if this one is the later. For this purpose, dateTime values with an unknown
   *         timezone are considered to be UTC values (the Comparable interface requires
   *         a total ordering).
   * @throws ClassCastException if the other value is not a TimeValue (the parameter
   *                            is declared as Object to satisfy the Comparable interface)
   */
  def compareTo(other: CalendarValue, implicitTimezone: Int): Int = {
    toDateTime().compareTo(other.asInstanceOf[TimeValue].toDateTime(), implicitTimezone)
  }

  override def equals(other: Any): Boolean = other match {
    case other: TimeValue ⇒ compareTo(other) == 0
    case _ ⇒ false
  }

  override def hashCode(): Int = {
    DateTimeValue.hashCode(1951, 10.toByte, 11.toByte, hour, minute, second, microsecond, getTimezoneInMinutes)
  }

  /**
   * Add a duration to a dateTime
   *
   * @param duration the duration to be added (may be negative)
   * @return the new date
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if the duration is an xs:duration, as distinct from
   *          a subclass thereof
   */
  override def add(duration: DurationValue): TimeValue = {
    if (duration.isInstanceOf[DayTimeDurationValue]) {
      val dt = toDateTime().add(duration)
      new TimeValue(dt.getHour, dt.getMinute, dt.getSecond, dt.getMicrosecond, getTimezoneInMinutes)
    } else {
      super.add(duration).asInstanceOf[TimeValue]
    }
  }

  /**
   * Determine the difference between two points in time, as a duration
   *
   * @param other   the other point in time
   * @param context XPath dynamic evaluation context
   * @return the duration as an xs:dayTimeDuration
   * @throws XPathException for example if one value is a date and the other is a time
   */
  override def subtract(other: CalendarValue, context: XPathContext): DayTimeDurationValue = {
    if (!other.isInstanceOf[TimeValue]) {
      throw new XPathException("First operand of '-' is a time, but the second is not", "XPTY0004")
    }
    super.subtract(other, context)
  }
}

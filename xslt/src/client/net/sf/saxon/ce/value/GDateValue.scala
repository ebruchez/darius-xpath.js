// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.functions.Component
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.ValidationFailure
import com.google.gwt.regexp.shared.MatchResult
import com.google.gwt.regexp.shared.RegExp
import GDateValue._
//remove if not needed
import scala.collection.JavaConversions._

object GDateValue {

  /**
   * Test whether a candidate date is actually a valid date in the proleptic Gregorian calendar
   */
  protected var daysPerMonth: Array[Byte] = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  protected val monthData = Array(306, 337, 0, 31, 61, 92, 122, 153, 184, 214, 245, 275)

  private var datePattern: RegExp = RegExp.compile("\\-?([0-9]+)-([0-9][0-9])-([0-9][0-9])([-+Z].*)?")

  protected def setLexicalValue(dt: GDateValue, s: CharSequence): ConversionResult = {
    val str = s.toString
    val `match` = datePattern.exec(str)
    if (`match` == null) {
      return badDate("wrong format", str)
    }
    dt.year = DurationValue.simpleInteger(`match`.getGroup(1))
    if (str.startsWith("-")) {
      dt.year = dt.year - 1
      dt.year = -dt.year
    }
    dt.month = DurationValue.simpleInteger(`match`.getGroup(2))
    dt.day = DurationValue.simpleInteger(`match`.getGroup(3))
    val tz = `match`.getGroup(4)
    val tzmin = parseTimezone(tz)
    if (tzmin == BAD_TIMEZONE) {
      return badDate("invalid timezone", str)
    }
    dt.setTimezoneInMinutes(tzmin)
    if (dt.year == 0) {
      return badDate("year zero", str)
    }
    if (!DateValue.isValidDate(dt.year, dt.month, dt.day)) {
      return badDate("non-existent date", s)
    }
    dt
  }

  private def badDate(msg: String, value: CharSequence): ValidationFailure = {
    new ValidationFailure("Invalid date " + Err.wrap(value, Err.VALUE) + " (" + 
      msg + 
      ")", "FORG0001")
  }

  /**
   * Determine whether a given date is valid
   * @param year the year (permitting year zero)
   * @param month the month (1-12)
   * @param day the day (1-31)
   * @return true if this is a valid date
   */
  def isValidDate(year: Int, month: Int, day: Int): Boolean = {
    month > 0 && month <= 12 && day > 0 && day <= daysPerMonth(month - 1) || 
      month == 2 && day == 29 && isLeapYear(year)
  }

  /**
   * Test whether a year is a leap year
   * @param year the year (permitting year zero)
   * @return true if the supplied year is a leap year
   */
  def isLeapYear(year: Int): Boolean = {
    (year % 4 == 0) && !(year % 100 == 0 && !(year % 400 == 0))
  }
}

/**
 * Abstract superclass for the primitive types containing date components: xs:date, xs:gYear,
 * xs:gYearMonth, xs:gMonth, xs:gMonthDay, xs:gDay
 */
abstract class GDateValue extends CalendarValue {

  protected var year: Int = _

  protected var month: Int = _

  protected var day: Int = _

  /**
   * Get the year component of the date (in local form)
   * @return the year component, as represented internally (allowing a year zero)
   */
  def getYear(): Int = year

  /**
   * Get the month component of the date (in local form)
   * @return the month component (1-12)
   */
  def getMonth(): Int = month

  /**
   * Get the day component of the date (in local form)
   * @return the day component (1-31)
   */
  def getDay(): Int = day

  /**
   * The equals() methods on atomic values is defined to follow the semantics of eq when applied
   * to two atomic values. When the other operand is not an atomic value, the result is undefined
   * (may be false, may be an exception). When the other operand is an atomic value that cannot be
   * compared with this one, the method returns false.
   * <p/>
   * <p>The hashCode() method is consistent with equals().</p>
   *
   * <p>This implementation performs a context-free comparison: it fails with ClassCastException
   * if one value has a timezone and the other does not.</p>
   *
   * @param o the other value
   * @return true if the other operand is an atomic value and the two values are equal as defined
   *         by the XPath eq operator
   * @throws ClassCastException if the values are not comparable
   */
  override def equals(o: Any): Boolean = o match {
    case o: GDateValue => {
      val gdv = o
      getItemType == gdv.getItemType && toDateTime() == gdv.toDateTime()
    }
    case _ => false
  }

  override def hashCode(): Int = {
    DateTimeValue.hashCode(year, month, day, 12, 0, 0, 0, getTimezoneInMinutes)
  }

  /**
   * Compare this value to another value of the same type, using the supplied context object
   * to get the implicit timezone if required. This method implements the XPath comparison semantics.
   *
   *
   * @param other the value to be compared
   * @param implicitTimezone timezone to be used if there is none in the value
   * @return -1 if this value is less, 0 if equal, +1 if greater
   */
  def compareTo(other: CalendarValue, implicitTimezone: Int): Int = {
    if (getItemType != other.getItemType) {
      throw new ClassCastException("Cannot compare dates of different types")
    }
    toDateTime().compareTo(other.toDateTime(), implicitTimezone)
  }

  /**
   * Convert to DateTime.
   * @return the starting instant of the GDateValue (with the same timezone)
   */
  def toDateTime(): DateTimeValue = {
    new DateTimeValue(year, month, day, 0.toByte, 0.toByte, 0.toByte, 0, getTimezoneInMinutes)
  }

  /**
   * Return a new date, time, or dateTime with the same normalized value, but
   * in a different timezone
   *
   * @param tz the new timezone offset from UTC, in minutes
   * @return the date/time in the new timezone
   */
  override def adjustTimezone(tz: Int): CalendarValue = {
    toDateTime().adjustTimezone(tz).convert(getItemType).asInstanceOf[DateTimeValue]
  }

  /**
   * Get a component of the value. Returns null if the timezone component is
   * requested and is not present.
   */
  def getComponent(component: Int): AtomicValue = component match {
    case Component.YEAR => 
      var value = if (year > 0) year else year - 1
      new IntegerValue(value)

    case Component.MONTH => new IntegerValue(month)
    case Component.DAY => new IntegerValue(day)
    case Component.TIMEZONE => if (hasTimezone()) {
      DayTimeDurationValue.fromMilliseconds(60000L * getTimezoneInMinutes)
    } else {
      null
    }
    case _ => throw new IllegalArgumentException("Unknown component for date: " + component)
  }
}

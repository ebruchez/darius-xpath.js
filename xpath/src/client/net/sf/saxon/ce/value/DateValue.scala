// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.`type`.{AtomicType, ConversionResult, ValidationFailure}
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.FormatDate
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.DateValue._

object DateValue {

  /**
   * Static factory method: construct a DateValue from a string in the lexical form
   * of a date, returning a ValidationFailure if the supplied string is invalid
   *
   * @param in    the lexical form of the date
   * @return either a DateValue or a ValidationFailure
   */
  def makeDateValue(in: CharSequence): ConversionResult = GDateValue.setLexicalValue(new DateValue(), in)

  /**
   * Get the date that immediately follows a given date
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return a new DateValue with no timezone information
   */
  def tomorrow(year: Int, month: Int, day: Int): DateValue = {
    new DateValue(year, month, day).add(DayTimeDurationValue.ONE_DAY)
  }

  /**
   * Get the date that immediately precedes a given date
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return a new DateValue with no timezone information
   */
  def yesterday(year: Int, month: Int, day: Int): DateValue = {
    new DateValue(year, month, day).add(DayTimeDurationValue.MINUS_ONE_DAY)
  }

  /**
   * Calculate the Julian day number at 00:00 on a given date. This algorithm is taken from
   * http://vsg.cape.com/~pbaum/date/jdalg.htm and
   * http://vsg.cape.com/~pbaum/date/jdalg2.htm
   * (adjusted to handle BC dates correctly)
   * <p/>
   * <p>Note that this assumes dates in the proleptic Gregorian calendar</p>
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return the Julian day number
   */
  def getJulianDayNumber(year: Int, month: Int, day: Int): Int = {
    var z = year - (if (month < 3) 1 else 0)
    val f = GDateValue.monthData(month - 1)
    if (z >= 0) {
      day + f + 365 * z + z / 4 - z / 100 + z / 400 + 1721118
    } else {
      z += 12000
      val j = day + f + 365 * z + z / 4 - z / 100 + z / 400 + 1721118
      j - 
        (365 * 12000 + 12000 / 4 - 12000 / 100 + 12000 / 400)
    }
  }

  /**
   * Get the Gregorian date corresponding to a particular Julian day number. The algorithm
   * is taken from http://www.hermetic.ch/cal_stud/jdn.htm#comp
   *
   * @param julianDayNumber the Julian day number
   * @return a DateValue with no timezone information set
   */
  def dateFromJulianDayNumber(julianDayNumber: Int): DateValue = {
    if (julianDayNumber >= 0) {
      var L = julianDayNumber + 68569 + 1
      val n = (4 * L) / 146097
      L = L - (146097 * n + 3) / 4
      val i = (4000 * (L + 1)) / 1461001
      L = L - (1461 * i) / 4 + 31
      val j = (80 * L) / 2447
      val d = L - (2447 * j) / 80
      L = j / 11
      val m = j + 2 - (12 * L)
      val y = 100 * (n - 49) + i + L
      new DateValue(y, m.toByte, d.toByte)
    } else {
      val dt = dateFromJulianDayNumber(julianDayNumber + 
        (365 * 12000 + 12000 / 4 - 12000 / 100 + 12000 / 400))
      dt.year -= 12000
      dt
    }
  }

  /**
   * Get the ordinal day number within the year (1 Jan = 1, 1 Feb = 32, etc)
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return the ordinal day number within the year
   */
  def getDayWithinYear(year: Int, month: Int, day: Int): Int = {
    val j = getJulianDayNumber(year, month, day)
    val k = getJulianDayNumber(year, 1, 1)
    j - k + 1
  }

  /**
   * Get the day of the week.  The days of the week are numbered from
   * 1 (Monday) to 7 (Sunday)
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return the day of the week, 1=Monday .... 7=Sunday
   */
  def getDayOfWeek(year: Int, month: Int, day: Int): Int = {
    var d = getJulianDayNumber(year, month, day)
    d -= 2378500
    while (d <= 0) {
      d += 70000000
    }
    (d - 1) % 7 + 1
  }

  /**
   * Get the ISO week number for a given date.  The days of the week are numbered from
   * 1 (Monday) to 7 (Sunday), and week 1 in any calendar year is the week (from Monday to Sunday)
   * that includes the first Thursday of that year
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return the ISO week number
   */
  def getWeekNumber(year: Int, month: Int, day: Int): Int = {
    val d = getDayWithinYear(year, month, day)
    val firstDay = getDayOfWeek(year, 1, 1)
    if (firstDay > 4 && (firstDay + d) <= 8) {
      return getWeekNumber(year - 1, 12, 31)
    }
    val inc = if (firstDay < 5) 1 else 0
    ((d + firstDay - 2) / 7) + inc
  }

  /**
   * Get the week number within a month. This is required for the XSLT format-date() function,
   * and the rules are not entirely clear. The days of the week are numbered from
   * 1 (Monday) to 7 (Sunday), and by analogy with the ISO week number, we consider that week 1
   * in any calendar month is the week (from Monday to Sunday) that includes the first Thursday
   * of that month. Unlike the ISO week number, we put the previous days in week zero.
   *
   * @param year  the year
   * @param month the month (1-12)
   * @param day   the day (1-31)
   * @return the week number within a month
   */
  def getWeekNumberWithinMonth(year: Int, month: Int, day: Int): Int = {
    val firstDay = getDayOfWeek(year, month, 1)
    val inc = if (firstDay < 5) 1 else 0
    ((day + firstDay - 2) / 7) + inc
  }
}

/**
 * A value of type Date. Note that a Date may include a TimeZone.
 */
class DateValue private () extends GDateValue with Comparable[AnyRef] {

  /**
   * Constructor given a year, month, and day. Performs no validation.
   *
   * @param year  The year as held internally (note that the year before 1AD is supplied as 0,
   *              but will be displayed on output as -0001)
   * @param month The month, 1-12
   * @param day   The day, 1-31
   */
  def this(year: Int, month: Int, day: Int) {
    this()
    this.year = year
    this.month = month
    this.day = day
  }

  /**
   * Constructor given a year, month, and day, and timezone. Performs no validation.
   *
   * @param year  The year as held internally (note that the year before 1AD is 0)
   * @param month The month, 1-12
   * @param day   The day, 1-31
   * @param tz    the timezone displacement in minutes from UTC. Supply the value
   *              [[CalendarValue.NO_TIMEZONE]] if there is no timezone component.
   */
  def this(year: Int, 
      month: Int, 
      day: Int, 
      tz: Int) {
    this()
    this.year = year
    this.month = month
    this.day = day
    setTimezoneInMinutes(tz)
  }

  /**
   * Constructor: create a date value from a supplied string, in
   * ISO 8601 format
   *
   * @param s     the lexical form of the date value
   * @throws XPathException if the supplied string is not a valid date
   */
  def this(s: CharSequence) {
    this()
    GDateValue.setLexicalValue(this, s).asAtomic()
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.DATE

  /**
   * Convert to target data type
   *
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.DATE) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.DATE_TIME) {
      toDateTime()
    } else if (requiredType == AtomicType.G_YEAR) {
      new GYearValue(year, getTimezoneInMinutes)
    } else if (requiredType == AtomicType.G_YEAR_MONTH) {
      new GYearMonthValue(year, month, getTimezoneInMinutes)
    } else if (requiredType == AtomicType.G_MONTH) {
      new GMonthValue(month, getTimezoneInMinutes)
    } else if (requiredType == AtomicType.G_MONTH_DAY) {
      new GMonthDayValue(month, day, getTimezoneInMinutes)
    } else if (requiredType == AtomicType.G_DAY) {
      new GDayValue(day, getTimezoneInMinutes)
    } else {
      new ValidationFailure("Cannot convert date to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert to string
   *
   * @return ISO 8601 representation.
   */
  def getPrimitiveStringValue(): CharSequence = {
    FormatDate.formatDate(this, "[Y0001]-[M01]-[D01][Z]", "en")
  }

  /**
   * Make a copy of this date value, but with a new type label
   *
   * @return the new xs:date value
   */
  def copy(): AtomicValue = {
    new DateValue(year, month, day, getTimezoneInMinutes)
  }

  /**
   * Add a duration to a date
   *
   * @param duration the duration to be added (may be negative)
   * @return the new date
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if the duration is an xs:duration, as distinct from
   *          a subclass thereof
   */
  override def add(duration: DurationValue): DateValue = {
    if (duration.isInstanceOf[DayTimeDurationValue]) {
      var microseconds = duration.asInstanceOf[DayTimeDurationValue].getLengthInMicroseconds
      val negative = microseconds < 0
      microseconds = Math.abs(microseconds)
      val days = Math.floor(microseconds.toDouble / (1000000L * 60L * 60L * 24L)).toInt
      val partDay = (microseconds % (1000000L * 60L * 60L * 24L)) > 0
      val julian = getJulianDayNumber(year, month, day)
      var d = dateFromJulianDayNumber(julian + (if (negative) -days else days))
      if (partDay) {
        if (negative) {
          d = yesterday(d.year, d.month, d.day)
        }
      }
      d.setTimezoneInMinutes(getTimezoneInMinutes)
      d
    } else if (duration.isInstanceOf[YearMonthDurationValue]) {
      val months = duration.asInstanceOf[YearMonthDurationValue].getLengthInMonths
      var m = (month - 1) + months
      var y = year + m / 12
      m = m % 12
      if (m < 0) {
        m += 12
        y -= 1
      }
      m += 1
      var d = day
      while (! GDateValue.isValidDate(y, m, d)) {
        d -= 1
      }
      new DateValue(y, m.toByte, d.toByte, getTimezoneInMinutes)
    } else {
      super.add(duration).asInstanceOf[DateValue]
    }
  }

  /**
   * Determine the difference between two points in time, as a duration
   *
   * @param other   the other point in time
   * @param context the XPath dynamic context
   * @return the duration as an xs:dayTimeDuration
   * @throws XPathException for example if one value is a date and the other is a time
   */
  override def subtract(other: CalendarValue, context: XPathContext): DayTimeDurationValue = {
    if (!other.isInstanceOf[DateValue]) {
      throw new XPathException("First operand of '-' is a date, but the second is not", "XPTY0004")
    }
    super.subtract(other, context)
  }

  /**
   * Context-free comparison of two DateValue values. For this to work,
   * the two values must either both have a timezone or both have none.
   *
   * @param v2 the other value
   * @return the result of the comparison: -1 if the first is earlier, 0 if they
   *         are equal, +1 if the first is later
   * @throws ClassCastException if the values are not comparable (which might be because
   *                            no timezone is available)
   */
  def compareTo(v2: AnyRef): Int = compareTo(v2.asInstanceOf[DateValue])
}

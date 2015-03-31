// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import com.google.gwt.regexp.shared.MatchResult
import com.google.gwt.regexp.shared.RegExp
import java.math.BigDecimal
import CalendarValue._
//remove if not needed
import scala.collection.JavaConversions._

object CalendarValue {

  val NO_TIMEZONE = Integer.MIN_VALUE

  val BAD_TIMEZONE = Integer.MAX_VALUE

  private var timezonePattern: RegExp = RegExp.compile("[-+]([0-9][0-9]):([0-9][0-9])")

  def parseTimezone(zone: String): Int = {
    if (zone == null || zone.isEmpty) {
      NO_TIMEZONE
    } else if (zone == "Z") {
      0
    } else {
      val `match` = timezonePattern.exec(zone)
      if (`match` == null) {
        return BAD_TIMEZONE
      }
      val h = DurationValue.simpleInteger(`match`.getGroup(1))
      val m = DurationValue.simpleInteger(`match`.getGroup(2))
      if (h > 14 || (h == 14 && m > 0)) {
        return BAD_TIMEZONE
      }
      var tz = h * 60 + m
      if (zone.charAt(0) == '-') {
        tz = -tz
      }
      tz
    }
  }

  /**
   * Append an integer, formatted with leading zeros to a fixed size, to a string buffer
   *
   * @param sb    the string buffer
   * @param value the integer to be formatted
   * @param size  the number of digits required (max 9)
   */
  def appendString(sb: FastStringBuffer, value: Int, size: Int): Unit = {
    val s = "000000000" + value
    sb.append(s.substring(s.length - size))
  }

  /**
   * Append an integer, formatted as two digits, to a string buffer
   *
   * @param sb    the string buffer
   * @param value the integer to be formatted (must be in the range 0..99
   */
  def appendTwoDigits(sb: FastStringBuffer, value: Int): Unit = {
    sb.append((value / 10 + '0').toChar)
    sb.append((value % 10 + '0').toChar)
  }
}

/**
 * Abstract superclass for Date, Time, and DateTime.
 */
abstract class CalendarValue extends AtomicValue {

  private var tzMinutes: Int = NO_TIMEZONE

  /**
   * Determine whether this value includes a timezone
   *
   * @return true if there is a timezone in the value, false if not
   */
  def hasTimezone(): Boolean = tzMinutes != NO_TIMEZONE

  /**
   * Modify the timezone value held in this object. This must be done only while the value is being
   * constructed.
   *
   * @param minutes The timezone offset from GMT in minutes, positive or negative; or the special
   *                value NO_TIMEZONE indicating that the value is not in a timezone (this is the default if this
   *                method is not called)
   */
  def setTimezoneInMinutes(minutes: Int): Unit = {
    tzMinutes = minutes
  }

  /**
   * Convert the value to a DateTime, retaining all the components that are actually present, and
   * substituting conventional values for components that are missing
   *
   * @return the equivalent DateTimeValue
   */
  def toDateTime(): DateTimeValue

  /**
   * Get the timezone value held in this object.
   *
   * @return The timezone offset from GMT in minutes, positive or negative; or the special
   *         value NO_TIMEZONE indicating that the value is not in a timezone
   */
  def getTimezoneInMinutes(): Int = tzMinutes

  /**
   * Add a duration to this date/time value
   *
   * @param duration the duration to be added (which might be negative)
   * @return a new date/time value representing the result of adding the duration. The original
   *         object is not modified.
   * @throws XPathException in the event of a dynamic error
   */
  def add(duration: DurationValue): CalendarValue = {
    throw new XPathException("Cannot add an " + duration.getItemType.getDisplayName + 
      " to an " + 
      getItemType.getDisplayName, "XPTY0004")
  }

  /**
   * Determine the difference between two points in time, as a duration
   *
   * @param other   the other point in time
   * @param context the dynamic context, used to obtain timezone information. May be set to null
   *                only if both values contain an explicit timezone, or if neither does so.
   * @return the duration as an xs:dayTimeDuration
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          for example if one value is a date and the other is a time
   */
  def subtract(other: CalendarValue, context: XPathContext): DayTimeDurationValue = {
    var dt1 = toDateTime()
    var dt2 = other.toDateTime()
    if (dt1.getTimezoneInMinutes != dt2.getTimezoneInMinutes) {
      dt1 = dt1.normalize(context)
      dt2 = dt2.normalize(context)
    }
    val d1 = dt1.toJulianInstant()
    val d2 = dt2.toJulianInstant()
    val difference = d1.subtract(d2)
    DayTimeDurationValue.fromSeconds(difference)
  }

  /**
   * Create a copy of this atomic value
   *
   * @return the copied value
   */
  protected def copy(): AtomicValue

  /**
   * Return a date, time, or dateTime with the same localized value, but
   * without the timezone component
   *
   * @return the result of removing the timezone
   */
  def removeTimezone(): CalendarValue = {
    val c = copy().asInstanceOf[CalendarValue]
    c.tzMinutes = NO_TIMEZONE
    c
  }

  /**
   * Return a new date, time, or dateTime with the same normalized value, but
   * in a different timezone
   *
   * @param tz the new timezone offset from UTC, in minutes
   * @return the date/time in the new timezone
   */
  def adjustTimezone(tz: Int): CalendarValue

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
   * @param ordered          true if an ordered comparison is required. In this case the result is null if the
   *                         type is unordered; in other cases the returned value will be a Comparable.
   * @param collator         collation used for strings
   * @param implicitTimezone the implicit timezone, needed when comparing date/time values
   * @return an Object whose equals() and hashCode() methods implement the XPath comparison semantics
   */
  def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    if (ordered && !this.isInstanceOf[Comparable[_]]) {
      return null
    }
    if (hasTimezone()) this else adjustTimezone(implicitTimezone)
  }

  /**
   * Compare this value to another value of the same type, using the supplied Configuration
   * to get the implicit timezone if required.
   *
   * @param other            the other value to be compared
   * @param implicitTimezone from the dynamic context
   * @return the comparison result
   */
  def compareTo(other: CalendarValue, implicitTimezone: Int): Int

  /**
   * Add a string representation of the timezone, typically
   * formatted as "Z" or "+03:00" or "-10:00", to a supplied
   * string buffer
   *
   * @param sb The StringBuffer that will be updated with the resulting string
   *           representation
   */
  def appendTimezone(sb: FastStringBuffer): Unit = {
    if (hasTimezone()) {
      var tz = getTimezoneInMinutes
      if (tz == 0) {
        sb.append("Z")
      } else {
        sb.append(if (tz > 0) "+" else "-")
        tz = Math.abs(tz)
        appendTwoDigits(sb, tz / 60)
        sb.append(':')
        appendTwoDigits(sb, tz % 60)
      }
    }
  }
}

package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.Component
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.ValidationFailure
import com.google.gwt.regexp.shared.MatchResult
import com.google.gwt.regexp.shared.RegExp
import java.math.BigDecimal
import java.math.BigInteger
import java.util.Date
import DateTimeValue._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object DateTimeValue {

  /**
   * Get the dateTime value representing the nominal
   * date/time of this transformation run. Two calls within the same
   * query or transformation will always return the same answer.
   *
   * @param context the XPath dynamic context. May be null, in which case
   * the current date and time are taken directly from the system clock
   * @return the current xs:dateTime
   */
  def getCurrentDateTime(context: XPathContext): DateTimeValue = {
    var c: Controller = null
    if (context == null || (c = context.getController) == null) {
      DateTimeValue.fromJavaDate(new Date())
    } else {
      c.getCurrentDateTime
    }
  }

  /**
   * Factory method: create a dateTime value given a Java Date object. The returned dateTime
   * value will always have a timezone, which will always be UTC.
   *
   * @param suppliedDate holds the date and time
   * @return the corresponding xs:dateTime value
   */
  def fromJavaDate(suppliedDate: Date): DateTimeValue = {
    try {
      val millis = suppliedDate.getTime
      EPOCH.add(DayTimeDurationValue.fromMilliseconds(millis))
    } catch {
      case e: XPathException => EPOCH
    }
  }

  /**
   * Fixed date/time used by Java (and Unix) as the origin of the universe: 1970-01-01
   */
  val EPOCH = new DateTimeValue(1970, 1, 1, 0, 0, 0, 0, 0)

  /**
   * Factory method: create a dateTime value given a date and a time.
   *
   * @param date the date
   * @param time the time
   * @return the dateTime with the given components. If either component is null, returns null
   * @throws XPathException if the timezones are both present and inconsistent
   */
  def makeDateTimeValue(date: DateValue, time: TimeValue): DateTimeValue = {
    if (date == null || time == null) {
      return null
    }
    val tz1 = date.getTimezoneInMinutes
    val tz2 = time.getTimezoneInMinutes
    if (tz1 != NO_TIMEZONE && tz2 != NO_TIMEZONE && tz1 != tz2) {
      throw new XPathException("Supplied date and time are in different timezones", "FORG0008")
    }
    val v = date.toDateTime()
    v.hour = time.getHour
    v.minute = time.getMinute
    v.second = time.getSecond
    v.microsecond = time.getMicrosecond
    v.setTimezoneInMinutes(Math.max(tz1, tz2))
    v
  }

  private var dateTimePattern: RegExp = RegExp.compile("^\\-?([0-9][0-9][0-9][0-9][0-9]*)-([0-9][0-9])-([0-9][0-9])T([0-2][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]*)?([-+Z].*)?$")

  def makeDateTimeValue(s: CharSequence): ConversionResult = {
    val str = s.toString
    val `match` = dateTimePattern.exec(str)
    if (`match` == null) {
      return badDate("wrong format", str)
    }
    val dt = new DateTimeValue()
    dt.year = DurationValue.simpleInteger(`match`.getGroup(1))
    if (str.startsWith("-")) {
      dt.year = dt.year - 1
      dt.year = -dt.year
    }
    dt.month = DurationValue.simpleInteger(`match`.getGroup(2))
    dt.day = DurationValue.simpleInteger(`match`.getGroup(3))
    dt.hour = DurationValue.simpleInteger(`match`.getGroup(4))
    dt.minute = DurationValue.simpleInteger(`match`.getGroup(5))
    dt.second = DurationValue.simpleInteger(`match`.getGroup(6))
    val frac = `match`.getGroup(7)
    if (frac != null && frac.length > 0) {
      val fractionalSeconds = Double.parseDouble(frac)
      dt.microsecond = (Math.round(fractionalSeconds * 1000000)).toInt
    }
    val tz = `match`.getGroup(8)
    val tzmin = parseTimezone(tz)
    if (tzmin == BAD_TIMEZONE) {
      return badDate("Invalid timezone", str)
    }
    dt.setTimezoneInMinutes(tzmin)
    if (dt.year == 0) {
      return badDate("year zero", str)
    }
    if (!DateValue.isValidDate(dt.year, dt.month, dt.day)) {
      return badDate("Non-existent date", s)
    }
    if (dt.hour == 24) {
      if (dt.minute != 0 || dt.second != 0 || dt.microsecond != 0) {
        return badDate("after midnight", str)
      } else {
        dt.hour = 0
        val tomorrow = DateValue.tomorrow(dt.year, dt.month, dt.day)
        dt.year = tomorrow.getYear
        dt.month = tomorrow.getMonth
        dt.day = tomorrow.getDay
      }
    }
    dt
  }

  private def badDate(msg: String, value: CharSequence): ValidationFailure = {
    new ValidationFailure("Invalid dateTime value " + Err.wrap(value, Err.VALUE) + 
      " (" + 
      msg + 
      ")", "FORG0001")
  }

  /**
   * Get the DateTimeValue corresponding to a given Julian instant
   *
   * @param instant the Julian instant: a decimal value whose integer part is the Julian day number
   *                multiplied by the number of seconds per day, and whose fractional part is the fraction of the second.
   * @return the xs:dateTime value corresponding to the Julian instant. This will always be in timezone Z.
   */
  def fromJulianInstant(instant: BigDecimal): DateTimeValue = {
    val julianSecond = instant.toBigInteger()
    val microseconds = instant.subtract(new BigDecimal(julianSecond)).multiply(DecimalValue.BIG_DECIMAL_ONE_MILLION)
    var js = julianSecond.longValue()
    val jd = js / (24L * 60L * 60L)
    val date = DateValue.dateFromJulianDayNumber(jd.toInt)
    js = js % (24L * 60L * 60L)
    val hour = (js / (60L * 60L)).toByte
    js = js % (60L * 60L)
    val minute = (js / (60L)).toByte
    js = js % (60L)
    new DateTimeValue(date.getYear, date.getMonth, date.getDay, hour, minute, js.toByte, microseconds.intValue(), 
      0)
  }

  def hashCode(year: Int, 
      month: Int, 
      day: Int, 
      hour: Int, 
      minute: Int, 
      second: Int, 
      microsecond: Int, 
      tzMinutes: Int): Int = {
    val tz = -tzMinutes
    var h = hour
    var mi = minute
    mi += tz
    if (mi < 0 || mi > 59) {
      h += Math.floor(mi / 60.0)
      mi = (mi + 60 * 24) % 60
    }
    while (h < 0) {
      h += 24
      val t = DateValue.yesterday(year, month, day)
      year = t.getYear
      month = t.getMonth
      day = t.getDay
    }
    while (h > 23) {
      h -= 24
      val t = DateValue.tomorrow(year, month, day)
      year = t.getYear
      month = t.getMonth
      day = t.getDay
    }
    (year << 4) ^ (month << 28) ^ (day << 23) ^ (h << 18) ^ 
      (mi << 13) ^ 
      second ^ 
      microsecond
  }
}

/**
 * A value of type DateTime
 */
class DateTimeValue private () extends CalendarValue with Comparable[_] {

  @BeanProperty
  var year: Int = _

  @BeanProperty
  var month: Int = _

  @BeanProperty
  var day: Int = _

  @BeanProperty
  var hour: Int = _

  @BeanProperty
  var minute: Int = _

  @BeanProperty
  var second: Int = _

  @BeanProperty
  var microsecond: Int = _

  /**
   * Constructor: construct a DateTimeValue from its components.
   * This constructor performs no validation.
   *
   * @param year        The year as held internally (note that the year before 1AD is 0)
   * @param month       The month, 1-12
   * @param day         The day 1-31
   * @param hour        the hour value, 0-23
   * @param minute      the minutes value, 0-59
   * @param second      the seconds value, 0-59
   * @param microsecond the number of microseconds, 0-999999
   * @param tz          the timezone displacement in minutes from UTC. Supply the value
   *                    {@link CalendarValue#NO_TIMEZONE} if there is no timezone component.
   */
  def this(year: Int, 
      month: Int, 
      day: Int, 
      hour: Int, 
      minute: Int, 
      second: Int, 
      microsecond: Int, 
      tz: Int) {
    this()
    this.year = year
    this.month = month
    this.day = day
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
  def getItemType(): AtomicType = AtomicType.DATE_TIME

  /**
   * Convert the value to a DateTime, retaining all the components that are actually present, and
   * substituting conventional values for components that are missing. (This method does nothing in
   * the case of xs:dateTime, but is there to implement a method in the {@link CalendarValue} interface).
   *
   * @return the value as an xs:dateTime
   */
  def toDateTime(): DateTimeValue = this

  /**
   * Normalize the date and time to be in timezone Z.
   *
   * @param cc used to supply the implicit timezone, used when the value has
   *           no explicit timezone
   * @return in general, a new DateTimeValue in timezone Z, representing the same instant in time.
   *         Returns the original DateTimeValue if this is already in timezone Z.
   */
  def normalize(cc: XPathContext): DateTimeValue = {
    if (hasTimezone()) {
      adjustTimezone(0).asInstanceOf[DateTimeValue]
    } else {
      val dt = copy().asInstanceOf[DateTimeValue]
      dt.setTimezoneInMinutes(cc.getImplicitTimezone)
      dt.adjustTimezone(0).asInstanceOf[DateTimeValue]
    }
  }

  /**
   * Get the Julian instant: a decimal value whose integer part is the Julian day number
   * multiplied by the number of seconds per day,
   * and whose fractional part is the fraction of the second.
   * This method operates on the local time, ignoring the timezone. The caller should call normalize()
   * before calling this method to get a normalized time.
   *
   * @return the Julian instant corresponding to this xs:dateTime value
   */
  def toJulianInstant(): BigDecimal = {
    val julianDay = DateValue.getJulianDayNumber(year, month, day)
    var julianSecond = julianDay * (24L * 60L * 60L)
    julianSecond += (((hour * 60L + minute) * 60L) + second)
    val j = BigDecimal.valueOf(julianSecond)
    if (microsecond == 0) {
      j
    } else {
      j.add(BigDecimal.valueOf(microsecond).divide(DecimalValue.BIG_DECIMAL_ONE_MILLION, 6, BigDecimal.ROUND_HALF_EVEN))
    }
  }

  /**
   * Convert to target data type
   *
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.DATE_TIME) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.DATE) {
      new DateValue(year, month, day, getTimezoneInMinutes)
    } else if (requiredType == AtomicType.TIME) {
      new TimeValue(hour, minute, second, microsecond, getTimezoneInMinutes)
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
      new ValidationFailure("Cannot convert dateTime to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert to string
   *
   * @return ISO 8601 representation. The value returned is the localized representation,
   *         that is it uses the timezone contained within the value itself.
   */
  def getPrimitiveStringValue(): CharSequence = {
    val sb = new FastStringBuffer(30)
    var yr = year
    if (year <= 0) {
      yr = -yr + 1
      if (yr != 0) {
        sb.append('-')
      }
    }
    appendString(sb, yr, (if (yr > 9999) (yr + "").length else 4))
    sb.append('-')
    appendTwoDigits(sb, month)
    sb.append('-')
    appendTwoDigits(sb, day)
    sb.append('T')
    appendTwoDigits(sb, hour)
    sb.append(':')
    appendTwoDigits(sb, minute)
    sb.append(':')
    appendTwoDigits(sb, second)
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
    if (hasTimezone()) {
      appendTimezone(sb)
    }
    sb
  }

  /**
   * Make a copy of this date, time, or dateTime value, but with a new type label
   *
   */
  def copy(): AtomicValue = {
    new DateTimeValue(year, month, day, hour, minute, second, microsecond, getTimezoneInMinutes)
  }

  /**
   * Return a new dateTime with the same normalized value, but
   * in a different timezone.
   *
   * @param timezone the new timezone offset, in minutes
   * @return the date/time in the new timezone. This will be a new DateTimeValue unless no change
   *         was required to the original value
   */
  def adjustTimezone(timezone: Int): CalendarValue = {
    if (!hasTimezone()) {
      val in = copy().asInstanceOf[CalendarValue]
      in.setTimezoneInMinutes(timezone)
      return in
    }
    val oldtz = getTimezoneInMinutes
    if (oldtz == timezone) {
      return this
    }
    val tz = timezone - oldtz
    var h = hour
    var mi = minute
    mi += tz
    if (mi < 0 || mi > 59) {
      h += Math.floor(mi / 60.0)
      mi = (mi + 60 * 24) % 60
    }
    if (h >= 0 && h < 24) {
      return new DateTimeValue(year, month, day, h.toByte, mi.toByte, second, microsecond, timezone)
    }
    var dt = this
    while (h < 0) {
      h += 24
      val t = DateValue.yesterday(dt.getYear, dt.getMonth, dt.getDay)
      dt = new DateTimeValue(t.getYear, t.getMonth, t.getDay, h.toByte, mi.toByte, second, microsecond, 
        timezone)
    }
    if (h > 23) {
      h -= 24
      val t = DateValue.tomorrow(year, month, day)
      return new DateTimeValue(t.getYear, t.getMonth, t.getDay, h.toByte, mi.toByte, second, microsecond, 
        timezone)
    }
    dt
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
  def add(duration: DurationValue): DateTimeValue = {
    if (duration.isInstanceOf[DayTimeDurationValue]) {
      val microseconds = duration.asInstanceOf[DayTimeDurationValue].getLengthInMicroseconds
      val seconds = BigDecimal.valueOf(microseconds).divide(DecimalValue.BIG_DECIMAL_ONE_MILLION, 6, 
        BigDecimal.ROUND_HALF_EVEN)
      var julian = toJulianInstant()
      julian = julian.add(seconds)
      val dt = fromJulianInstant(julian)
      dt.setTimezoneInMinutes(getTimezoneInMinutes)
      dt
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
      while (!DateValue.isValidDate(y, m, d)) {
        d -= 1
      }
      new DateTimeValue(y, m.toByte, d.toByte, hour, minute, second, microsecond, getTimezoneInMinutes)
    } else {
      super.add(duration).asInstanceOf[DateTimeValue]
    }
  }

  /**
   * Determine the difference between two points in time, as a duration
   *
   * @param other   the other point in time
   * @param context the XPath dynamic context
   * @return the duration as an xs:dayTimeDuration
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          for example if one value is a date and the other is a time
   */
  def subtract(other: CalendarValue, context: XPathContext): DayTimeDurationValue = {
    if (!(other.isInstanceOf[DateTimeValue])) {
      throw new XPathException("First operand of '-' is a dateTime, but the second is not", "XPTY0004")
    }
    super.subtract(other, context)
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
    case Component.HOURS => new IntegerValue(hour)
    case Component.MINUTES => new IntegerValue(minute)
    case Component.SECONDS => 
      var d = BigDecimal.valueOf(microsecond)
      d = d.divide(DecimalValue.BIG_DECIMAL_ONE_MILLION, 6, BigDecimal.ROUND_HALF_UP)
      d = d.add(BigDecimal.valueOf(second))
      new DecimalValue(d)

    case Component.WHOLE_SECONDS => new IntegerValue(second)
    case Component.MICROSECONDS => new IntegerValue(microsecond)
    case Component.TIMEZONE => if (hasTimezone()) {
      DayTimeDurationValue.fromMilliseconds(60000L * getTimezoneInMinutes)
    } else {
      null
    }
    case _ => throw new IllegalArgumentException("Unknown component for dateTime: " + component)
  }

  /**
   * Compare the value to another dateTime value, following the XPath comparison semantics
   *
   *
   *
   * @param other  The other dateTime value
   * @param implicitTimezone timezone to be used by default
   * @return negative value if this one is the earler, 0 if they are chronologically equal,
   *         positive value if this one is the later. For this purpose, dateTime values with an unknown
   *         timezone are considered to be values in the implicit timezone (the Comparable interface requires
   *         a total ordering).
   * @throws ClassCastException if the other value is not a DateTimeValue (the parameter
   *                            is declared as CalendarValue to satisfy the interface)
   */
  def compareTo(other: CalendarValue, implicitTimezone: Int): Int = {
    if (!(other.isInstanceOf[DateTimeValue])) {
      throw new ClassCastException("DateTime values are not comparable to " + other.getClass)
    }
    var v1 = (if (hasTimezone()) this else adjustTimezone(implicitTimezone)).asInstanceOf[DateTimeValue]
    var v2 = other.asInstanceOf[DateTimeValue]
    if (!v2.hasTimezone()) {
      v2 = v2.adjustTimezone(implicitTimezone).asInstanceOf[DateTimeValue]
    }
    if (v1.getTimezoneInMinutes != v2.getTimezoneInMinutes) {
      v1 = v1.adjustTimezone(v2.getTimezoneInMinutes).asInstanceOf[DateTimeValue]
    }
    if (v1.year != v2.year) {
      return IntegerValue.signum(v1.year - v2.year)
    }
    if (v1.month != v2.month) {
      return IntegerValue.signum(v1.month - v2.month)
    }
    if (v1.day != v2.day) {
      return IntegerValue.signum(v1.day - v2.day)
    }
    if (v1.hour != v2.hour) {
      return IntegerValue.signum(v1.hour - v2.hour)
    }
    if (v1.minute != v2.minute) {
      return IntegerValue.signum(v1.minute - v2.minute)
    }
    if (v1.second != v2.second) {
      return IntegerValue.signum(v1.second - v2.second)
    }
    if (v1.microsecond != v2.microsecond) {
      return IntegerValue.signum(v1.microsecond - v2.microsecond)
    }
    0
  }

  /**
   * Context-free comparison of two DateTimeValue values. For this to work,
   * the two values must either both have a timezone or both have none.
   * @param v2 the other value
   * @return the result of the comparison: -1 if the first is earlier, 0 if they
   * are equal, +1 if the first is later
   * @throws ClassCastException if the values are not comparable (which might be because
   * no timezone is available)
   */
  def compareTo(v2: AnyRef): Int = {
    compareTo(v2.asInstanceOf[DateTimeValue], 0)
  }

  /**
   * Context-free comparison of two dateTime values
   * @param o the other date time value
   * @return true if the two values represent the same instant in time
   * @throws ClassCastException if one of the values has a timezone and the other does not
   */
  override def equals(o: Any): Boolean = o match {
    case o: DateTimeValue => compareTo(o) == 0
    case _ => false
  }

  /**
   * Hash code for context-free comparison of date time values. Note that equality testing
   * and therefore hashCode() works only for values with a timezone
   * @return  a hash code
   */
  override def hashCode(): Int = {
    hashCode(year, month, day, hour, minute, second, microsecond, getTimezoneInMinutes)
  }
}

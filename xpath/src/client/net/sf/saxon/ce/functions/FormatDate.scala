// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import java.math.BigDecimal

import client.net.sf.saxon.ce.expr.number.Numberer_en
import client.net.sf.saxon.ce.expr.{ExpressionVisitor, XPathContext}
import client.net.sf.saxon.ce.functions.FormatDate._
import client.net.sf.saxon.ce.lib.Numberer
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.regex.RegExp
import client.net.sf.saxon.ce.trans.{Err, XPathException}
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.{StringValue, _}

object FormatDate {

  /**
   * This method analyzes the formatting picture and delegates the work of formatting
   * individual parts of the date.
   * @param value    the value to be formatted
   * @param format   the supplied format picture
   * @param _language the chosen language
   * @return the formatted date/time
   * @throws XPathException if a dynamic error occurs
   */
  def formatDate(value: CalendarValue, format: String, _language: String): CharSequence = {
    var language = _language
    val languageDefaulted = language == null
    if (language == null) {
      language = "en"
    }
    val numberer = new Numberer_en()
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    if ("en" != language && !languageDefaulted) {
      sb.append("[Language: en]")
    }
    var i = 0
    while (true) {
      while (i < format.length && format.charAt(i) != '[') {
        val c = format.charAt(i)
        sb.append(c)
        if (c == ']') {
          i += 1
          if (i == format.length || format.charAt(i) != ']') {
            throw new XPathException("Closing ']' in date picture must be written as ']]'", "XTDE1340")
          }
        }
        i += 1
      }
      if (i == format.length) {
        //break
      }
      i += 1
      if (i < format.length && format.charAt(i) == '[') {
        sb.append('[')
        i += 1
      } else {
        val close = if (i < format.length) format.indexOf("]", i) else -1
        if (close == -1) {
          throw new XPathException("Date format contains a '[' with no matching ']'", "XTDE1340")
        }
        val componentFormat = format.substring(i, close)
        sb.append(formatComponent(value, Whitespace.removeAllWhitespace(componentFormat), numberer))
        i = close + 1
      }
    }
    sb
  }

  private val componentPattern = RegExp.compile("([YMDdWwFHhmsfZzPCE])\\s*(.*)")

  private def formatComponent(value: CalendarValue, specifier: CharSequence, numberer: Numberer): CharSequence = {
    val dtvalue = value.toDateTime
    val matcher = componentPattern.exec(specifier.toString)
    if (matcher == null) {
      throw new XPathException("Unrecognized date/time component [" + specifier + ']', "XTDE1340")
    }
    val component: Char = matcher.getGroup(1).charAt(0)
    var format: String = matcher.getGroup(2)
    if (format == null) {
      format = ""
    }
    var defaultFormat = false
    if ("" == format || format.startsWith(",")) {
      defaultFormat = true
      var use: String = null
      component match {
        case 'F' ⇒ use = "Nn"
        case 'P' ⇒ use = "n"
        case 'C' | 'E' ⇒ use = "N"
        case 'm' | 's' ⇒ use = "01"
        case _ ⇒ use = "1"
      }
      format = use + format
    }
    value match {
      case _: TimeValue if "YMDdWwFE".indexOf(component) >= 0 ⇒
        throw new XPathException("In formatTime(): an xs:time value does not contain component " +
          component, "XTDE1350")
      case _: DateValue if "hmsfP".indexOf(component) >= 0 ⇒
        throw new XPathException("In formatTime(): an xs:date value does not contain component " +
          component, "XTDE1350")
      case _ ⇒
    }
    var componentValue: Int = 0
    component match {
      case 'Y' ⇒
        componentValue = dtvalue.getYear
        if (componentValue < 0) {
          componentValue = 1 - componentValue
        }

      case 'M' ⇒ componentValue = dtvalue.getMonth
      case 'D' ⇒ componentValue = dtvalue.getDay
      case 'd' ⇒ componentValue = DateValue.getDayWithinYear(dtvalue.getYear, dtvalue.getMonth, dtvalue.getDay)
      case 'W' ⇒ componentValue = DateValue.getWeekNumber(dtvalue.getYear, dtvalue.getMonth, dtvalue.getDay)
      case 'w' ⇒ componentValue = DateValue.getWeekNumberWithinMonth(dtvalue.getYear, dtvalue.getMonth,
        dtvalue.getDay)
      case 'H' ⇒ componentValue = dtvalue.getHour
      case 'h' ⇒
        componentValue = dtvalue.getHour
        if (componentValue > 12) {
          componentValue = componentValue - 12
        }
        if (componentValue == 0) {
          componentValue = 12
        }

      case 'm' ⇒ componentValue = dtvalue.getMinute
      case 's' ⇒ componentValue = dtvalue.getSecond
      case 'f' ⇒ componentValue = dtvalue.getMicrosecond
      case 'Z' | 'z' ⇒
        var sbz = new FastStringBuffer(8)
        if (component == 'z') {
          sbz.append("GMT")
        }
        dtvalue.appendTimezone(sbz)
        return sbz.toString

      case 'F' ⇒ componentValue = DateValue.getDayOfWeek(dtvalue.getYear, dtvalue.getMonth, dtvalue.getDay)
      case 'P' ⇒ componentValue = dtvalue.getHour * 60 + dtvalue.getMinute
      case 'C' ⇒ return numberer.getCalendarName("AD")
      case 'E' ⇒ return numberer.getEraName(dtvalue.getYear)
      case _ ⇒ throw new XPathException("Unknown formatDate/time component specifier '" + format.charAt(0) +
        '\'', "XTDE1340")
    }
    formatNumber(component, componentValue, format, defaultFormat, numberer)
  }

  private val formatPattern = RegExp.compile("([^,]*)(,.*)?")
  private val widthPattern = RegExp.compile(",(\\*|[0-9]+)(\\-(\\*|[0-9]+))?")
  private val alphanumericPattern = RegExp.compile("([A-Za-z0-9])*")
  private val digitsPattern = RegExp.compile("[0-9]+")

  private def formatNumber(component: Char,
      value: Int,
      format: String,
      defaultFormat: Boolean,
      numberer: Numberer): CharSequence = {
    val matcher = formatPattern.exec(format)
    if (matcher == null) {
      throw new XPathException("Unrecognized format picture [" + component + format +
        ']', "XTDE1340")
    }
    var primary: String = matcher.getGroup(1)
    if (primary == null) {
      primary = ""
    }
    var modifier: String = null
    if (primary.endsWith("t")) {
      primary = primary.substring(0, primary.length - 1)
      modifier = "t"
    } else if (primary.endsWith("o")) {
      primary = primary.substring(0, primary.length - 1)
      modifier = "o"
    }
    val letterValue = if ("t" == modifier) "traditional" else null
    val ordinal = if ("o" == modifier) numberer.getOrdinalSuffixForDateTime(component) else null
    var widths: String = matcher.getGroup(2)
    if (widths == null) {
      widths = ""
    }
    if (! alphanumericPattern.test(primary)) {
      throw new XPathException("In format picture at '" + primary + "', primary format must be alphanumeric",
        "XTDE1340")
    }
    var min = 1
    var max = Integer.MAX_VALUE
    if (widths == null || "" == widths) {
      if (digitsPattern.test(primary)) {
        val len = StringValue.getStringLength(primary)
        if (len > 1) {
          min = len
          max = len
        }
      }
    } else if (primary == "I" || primary == "i") {
      min = 1
      max = Integer.MAX_VALUE
    } else {
      val range = getWidths(widths)
      min = range(0)
      max = range(1)
      if (defaultFormat) {
        if (primary.endsWith("1") && min != primary.length) {
          val sb = new FastStringBuffer(min + 1)
          for (i ← 1 until min) {
            sb.append('0')
          }
          sb.append('1')
          primary = sb.toString
        }
      }
    }
    if (component == 'P') {
      if (!("N" == primary || "Nn" == primary)) {
        primary = "n"
      }
      if (max == Integer.MAX_VALUE) {
        max = 4
      }
    } else if (component == 'f') {
      var s: String = null
      if (value == 0) {
        s = "0"
      } else {
        s = ((1000000 + value) + "").substring(1)
        if (s.length > max) {
          var dec = new DecimalValue(new BigDecimal("0." + s))
          dec = dec.roundHalfToEven(max).asInstanceOf[DecimalValue]
          s = dec.getStringValue
          s = if (s.length > 2) s.substring(2) else ""
        }
      }
      while (s.length < min) {
        s = s + '0'
      }
      while (s.length > min && s.charAt(s.length - 1) == '0') {
        s = s.substring(0, s.length - 1)
      }
      return s
    }
    if ("N" == primary || "n" == primary || "Nn" == primary) {
      var s = ""
      if (component == 'M') {
        s = numberer.monthName(value, min, max)
      } else if (component == 'F') {
        s = numberer.dayName(value, min, max)
      } else if (component == 'P') {
        s = numberer.halfDayName(value, min, max)
      } else {
        primary = "1"
      }
      if ("N" == primary) {
        return s.toUpperCase()
      } else if ("n" == primary) {
        return s.toLowerCase()
      } else {
        return s
      }
    }
    var s = numberer.format(value, primary, null, letterValue, ordinal)
    var len = StringValue.getStringLength(s)
    while (len < min) {
      s = ("00000000" + s).substring(s.length + 8 - min)
      len = StringValue.getStringLength(s)
    }
    if (len > max) {
      if (component == 'Y') {
        s = if (len == s.length) s.substring(s.length - max) else s.substring(s.length - 2 * max)
      }
    }
    s
  }

  private def getWidths(widths: String): Array[Int] = {
    var min = -1
    var max = -1
    if ("" != widths) {
      val widthMatcher = widthPattern.exec(widths)
      if (widthMatcher != null) {
        val smin: String = widthMatcher.getGroup(1)
        min = if (smin == null || "" == smin || "*" == smin) 1 else Integer.parseInt(smin)
        val smax: String = widthMatcher.getGroup(3)
        max = if (smax == null || "" == smax || "*" == smax) Integer.MAX_VALUE else Integer.parseInt(smax)
      } else {
        throw new XPathException("Unrecognized width specifier " + Err.wrap(widths, Err.VALUE), "XTDE1340")
      }
    }
    if (min > max && max != -1) {
      throw new XPathException("Minimum width in date/time picture exceeds maximum width", "XTDE1340")
    }
    val result = new Array[Int](2)
    result(0) = min
    result(1) = max
    result
  }
}

/**
 * Implement the format-date(), format-time(), and format-dateTime() functions
 * in XSLT 2.0 and XQuery 1.1.
 */
class FormatDate extends SystemFunction {

  def newInstance(): FormatDate = new FormatDate()

  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    val numArgs = argument.length
    if (numArgs != 2 && numArgs != 5) {
      throw new XPathException("Function " + getDisplayName + " must have either two or five arguments",
        getSourceLocator)
    }
    super.checkArguments(visitor)
  }

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(context: XPathContext): Item = {
    val value = argument(0).evaluateItem(context).asInstanceOf[CalendarValue]
    if (value == null) {
      return null
    }
    val format = argument(1).evaluateItem(context).getStringValue
    var calendarVal: StringValue = null
    var languageVal: StringValue = null
    if (argument.length > 2) {
      languageVal = argument(2).evaluateItem(context).asInstanceOf[StringValue]
      calendarVal = argument(3).evaluateItem(context).asInstanceOf[StringValue]
    }
    val language = if (languageVal == null) null else languageVal.getStringValue
    var result = formatDate(value, format, language)
    if (calendarVal != null) {
      val cal = calendarVal.getStringValue
      if (cal != "AD" && cal != "ISO") {
        result = "[Calendar: AD]" + result.toString
      }
    }
    new StringValue(result)
  }
}

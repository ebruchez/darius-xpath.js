// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import java.math.BigDecimal

import org.orbeon.darius.xpath.`type`.{AtomicType, ConversionResult, ValidationFailure}
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.regex.ARegularExpression
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.value.YearMonthDurationValue._

object YearMonthDurationValue {

  private val YMDdurationPattern: ARegularExpression = ARegularExpression.make("-?P([0-9]+Y)?([0-9]+M)?([0-9]+D)?")

  /**
   * Static factory: create a duration value from a supplied string, in
   * ISO 8601 format [+|-]PnYnM
   *
   * @param s a string in the lexical space of xs:yearMonthDuration.
   * @return either a YearMonthDurationValue, or a ValidationFailure if the string was
   *         not in the lexical space of xs:yearMonthDuration.
   */
  def makeYearMonthDurationValue(s: CharSequence): ConversionResult = {
    val d = DurationValue.makeDuration(s, YMDdurationPattern)
    if (d.isInstanceOf[ValidationFailure]) {
      return d
    }
    val dv = d.asInstanceOf[DurationValue]
    YearMonthDurationValue.fromMonths((dv.getYears * 12 + dv.getMonths) * (if (dv.isNegative) -1 else +1))
  }

  /**
   * Construct a duration value as a number of months.
   *
   * @param months the number of months (may be negative)
   * @return the corresponding xs:yearMonthDuration value
   */
  def fromMonths(months: Int): YearMonthDurationValue = {
    val mdv = new YearMonthDurationValue()
    mdv.negative = months < 0
    mdv.months = if (months < 0) -months else months
    mdv.seconds = 0
    mdv.microseconds = 0
    mdv
  }
}

/**
 * A value of type xs:yearMonthDuration
 */
class YearMonthDurationValue private () extends DurationValue with Comparable[AnyRef] {

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  override def getItemType: AtomicType = AtomicType.YEAR_MONTH_DURATION

  /**
   * Convert to string
   *
   * @return ISO 8601 representation.
   */
  override def getPrimitiveStringValue: CharSequence = {
    val y = getYears
    val m = getMonths
    val sb = new FastStringBuffer(32)
    if (negative) {
      sb.append('-')
    }
    sb.append('P')
    if (y != 0) {
      sb.append(y + "Y")
    }
    if (m != 0 || y == 0) {
      sb.append(m + "M")
    }
    sb
  }

  /**
   * Get the number of months in the duration
   *
   * @return the number of months in the duration
   */
  def getLengthInMonths: Int = months * (if (negative) -1 else +1)

  /**
   * Multiply duration by a number. Also used when dividing a duration by a number
   */
  override def multiply(n: Double): DurationValue = {
    if (n.isNaN) {
      throw new XPathException("Cannot multiply/divide a duration by NaN", "FOCA0005")
    }
    val m = getLengthInMonths.toDouble
    val product = n * m
    if (product.isInfinite || product > Integer.MAX_VALUE ||
      product < Integer.MIN_VALUE) {
      throw new XPathException("Overflow when multiplying/dividing a duration by a number", "FODT0002")
    }
    fromMonths(Math.round(product).toInt)
  }

  /**
   * Find the ratio between two durations
   *
   * @param other the dividend
   * @return the ratio, as a decimal
   * @throws XPathException
   */
  override def divide(other: DurationValue): DecimalValue = {
    other match {
      case value: YearMonthDurationValue ⇒
        val v1 = BigDecimal.valueOf(getLengthInMonths)
        val v2 = BigDecimal.valueOf(value.getLengthInMonths)
        if (v2.signum() == 0) {
          val err = new XPathException("Divide by zero (durations)")
          err.setErrorCode("FOAR0001")
          throw err
        }
        new DecimalValue(v1.divide(v2, 20, BigDecimal.ROUND_HALF_EVEN))
      case _ ⇒
        val err = new XPathException("Cannot divide two durations of different type")
        err.setErrorCode("XPTY0004")
        throw err
    }
  }

  /**
   * Add two year-month-durations
   */
  override def add(other: DurationValue): DurationValue = {
    other match {
      case value: YearMonthDurationValue ⇒
        fromMonths(getLengthInMonths +
          value.getLengthInMonths)
      case _ ⇒
        val err = new XPathException("Cannot add two durations of different type")
        err.setErrorCode("XPTY0004")
        throw err
    }
  }

  /**
   * Negate a duration (same as subtracting from zero, but it preserves the type of the original duration)
   */
  override def negate(): DurationValue = fromMonths(-getLengthInMonths)

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
    other match {
      case value: YearMonthDurationValue ⇒
        getLengthInMonths -
          value.getLengthInMonths
      case _ ⇒
        throw new ClassCastException("Cannot compare a yearMonthDuration to an object of class " +
          other.getClass)
    }
  }

  /**
   * Get a Comparable value that implements the XPath ordering comparison semantics for this value.
   * Returns null if the value is not comparable according to XPath rules. The default implementation
   * returns the value itself. This is modified for types such as
   * xs:duration which allow ordering comparisons in XML Schema, but not in XPath.
   * @param ordered
   * @param collator
   * @param implicitTimezone
   */
  override def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    this
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import java.math.BigDecimal

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.XPathException

object NumericValue {

  /**
   * Get a numeric value by parsing a string; the type of numeric value depends
   * on the lexical form of the string, following the rules for XPath numeric
   * literals.
   * @param in the input string
   * @return a NumericValue representing the value of the string. Returns Double.NaN if the
   * value cannot be parsed as a string.
   */
  def parseNumber(in: String): NumericValue = {
    if (in.indexOf('e') >= 0 || in.indexOf('E') >= 0) {
      try {
        new DoubleValue(in.toDouble)
      } catch {
        case e: NumberFormatException ⇒ DoubleValue.NaN
      }
    } else if (in.indexOf('.') >= 0) {
      val v = DecimalValue.makeDecimalValue(in)
      if (v.isInstanceOf[ValidationFailure]) {
        DoubleValue.NaN
      } else {
        v.asInstanceOf[NumericValue]
      }
    } else {
      val v = IntegerValue.stringToInteger(in)
      if (v.isInstanceOf[ValidationFailure]) {
        DoubleValue.NaN
      } else {
        v.asInstanceOf[NumericValue]
      }
    }
  }
}

/**
 * NumericValue is an abstract superclass for IntegerValue, DecimalValue,
 * FloatValue, and DoubleValue
 */
abstract class NumericValue extends AtomicValue with Comparable[AnyRef] {

  /**
   * Get the numeric value as a double
   *
   * @return A double representing this numeric value; NaN if it cannot be
   *     converted
   */
  def getDoubleValue(): Double = {
    try {
      convert(AtomicType.DOUBLE).asAtomic().asInstanceOf[DoubleValue]
        .getDoubleValue
    } catch {
      case err: XPathException ⇒ Double.NaN
    }
  }

  /**
   * Get the numeric value converted to a float
   * @return a float representing this numeric value; NaN if it cannot be converted
   */
  def getFloatValue(): Float = {
    try {
      convert(AtomicType.FLOAT).asAtomic().asInstanceOf[FloatValue]
        .getFloatValue
    } catch {
      case err: XPathException ⇒ Float.NaN
    }
  }

  /**
   * Get the numeric value converted to a decimal
   * @return a decimal representing this numeric value;
   * @throws XPathException if the value cannot be converted, for example if it is NaN or infinite
   */
  def getDecimalValue(): BigDecimal = {
    convert(AtomicType.DECIMAL).asAtomic().asInstanceOf[DecimalValue]
      .getDecimalValue
  }

  /**
   * Return the numeric value as a Java int.
   *
   * @throws client.net.sf.saxon.ce.trans.XPathException if the value is out of range
   * @return the numeric value as a Java int. This performs truncation
   *     towards zero.
   */
  def intValue(): Int = {
    convert(AtomicType.INTEGER).asAtomic().asInstanceOf[IntegerValue]
      .intValue()
  }

  /**
   * Change the sign of the number
   *
   * @return a value, of the same type as the original, with its sign
   *     inverted
   */
  def negate(): NumericValue

  /**
   * Implement the XPath floor() function
   *
   * @return a value, of the same type as that supplied, rounded towards
   *     minus infinity
   */
  def floor(): NumericValue

  /**
   * Implement the XPath ceiling() function
   *
   * @return a value, of the same type as that supplied, rounded towards
   *     plus infinity
   */
  def ceiling(): NumericValue

  /**
   * Implement the XPath round() function
   *
   * @return a value, of the same type as that supplied, rounded towards the
   *      nearest whole number (0.5 rounded up)
   */
  def round(): NumericValue

  /**
   * Implement the XPath 2.0 round-half-to-even() function
   *
   * @param scale the decimal position for rounding: e.g. 2 rounds to a
   *     multiple of 0.01, while -2 rounds to a multiple of 100
   * @return a value, of the same type as the original, rounded towards the
   *     nearest multiple of 10**(-scale), with rounding towards the nearest
   *      even number if two values are equally near
   */
  def roundHalfToEven(scale: Int): NumericValue

  /**
   * Determine whether the value is negative, zero, or positive
   * @return -1 if negative, 0 if zero (including negative zero), +1 if positive, NaN if NaN
   */
  def signum(): Double

  /**
   * Determine whether the value is a whole number, that is, whether it compares
   * equal to some integer
   *
   * @return true if the value is a whole number
   */
  def isWholeNumber(): Boolean

  /**
   * Get the absolute value as defined by the XPath abs() function
   * @return the absolute value
   * @since 9.2
   */
  def abs(): NumericValue

  /**
   * Get a Comparable value that implements the XPath ordering comparison semantics for this value.
   * Returns null if the value is not comparable according to XPath rules. The implementation
   * for all kinds of NumericValue returns the value itself.
   * @param ordered
   * @param collator
   * @param implicitTimezone
   */
  def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    this
  }

  def compareTo(other: AnyRef): Int = {
    val a = getDoubleValue
    val b = other.asInstanceOf[NumericValue].getDoubleValue
    if (a == b) return 0
    if (a < b) return -1
    +1
  }

  /**
   * Compare the value to a long
   * @param other the value to be compared with
   * @return -1 if this is less, 0 if this is equal, +1 if this is greater or if this is NaN
   */
  def compareTo(other: Long): Int

  /**
   * The equals() function compares numeric equality among integers, decimals, floats, doubles, and
   * their subtypes
   *
   * @param other the value to be compared with this one
   * @return true if the two values are numerically equal
   */
  override def equals(other: Any): Boolean = other match {
    case other: NumericValue ⇒ compareTo(other) == 0
    case _ ⇒ false
  }

  /**
   * hashCode() must be the same for two values that are equal. One
   * way to ensure this is to convert the value to a double, and take the
   * hashCode of the double. But this is expensive in the common case where
   * we are comparing integers. So we adopt the rule: for values that are in
   * the range of a Java Integer, we use the int value as the hashcode. For
   * values outside that range, we convert to a double and take the hashCode of
   * the double. This method needs to have a compatible implementation in
   * each subclass.
   *
   * @return the hash code of the numeric value
   */
  override def hashCode(): Int

  /**
   * Produce a string representation of the value
   * @return The result of casting the number to a string
   */
  override def toString(): String = getStringValue
}

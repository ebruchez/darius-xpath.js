// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.ValidationFailure
import java.math.BigDecimal
import FloatValue._
//remove if not needed
import scala.collection.JavaConversions._

object FloatValue {

  val ZERO = new FloatValue(0.0.toFloat)

  val NEGATIVE_ZERO = new FloatValue(-0.0.toFloat)

  val ONE = new FloatValue(1.0.toFloat)

  val NaN = new FloatValue(Float.NaN)
}

/**
 * A numeric (single precision floating point) value
 */
class FloatValue(var value: Float) extends NumericValue {

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.FLOAT

  /**
   * Get the value
   */
  def getFloatValue(): Float = value

  def getDoubleValue(): Double = value.toDouble

  /**
   * Get the hashCode. This must conform to the rules for other NumericValue hashcodes
   * @see NumericValue#hashCode
   */
  override def hashCode(): Int = {
    if (value > Integer.MIN_VALUE && value < Integer.MAX_VALUE) {
      value.toInt
    } else {
      new java.lang.Double(getDoubleValue).hashCode
    }
  }

  /**
   * Test whether the value is the double/float value NaN
   */
  def isNaN(): Boolean = Float.isNaN(value)

  /**
   * Get the effective boolean value
   * @return true unless the value is zero or NaN
   */
  def effectiveBooleanValue(): Boolean = value != 0.0 && !Float.isNaN(value)

  /**
   * Convert to target data type
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.NUMERIC || 
      requiredType == AtomicType.FLOAT) {
      this
    } else if (requiredType == AtomicType.BOOLEAN) {
      BooleanValue.get(effectiveBooleanValue())
    } else if (requiredType == AtomicType.INTEGER) {
      if (Float.isNaN(value)) {
        return new ValidationFailure("Cannot convert float NaN to an integer", "FOCA0002")
      }
      if (Float.isInfinite(value)) {
        return new ValidationFailure("Cannot convert float INF to an integer", "FOCA0002")
      }
      IntegerValue.decimalToInteger(new BigDecimal(value))
    } else if (requiredType == AtomicType.DECIMAL) {
      try {
        new DecimalValue(value)
      } catch {
        case e: XPathException => new ValidationFailure(e.getMessage)
      }
    } else if (requiredType == AtomicType.DOUBLE) {
      new DoubleValue(value)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else {
      new ValidationFailure("Cannot convert float to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert the double to a string according to the XPath 2.0 rules
   * @return the string value
   */
  def getPrimitiveStringValue(): CharSequence = {
    new DoubleValue(value).getPrimitiveStringValue
  }

  /**
   * Negate the value
   */
  def negate(): NumericValue = new FloatValue(-value)

  /**
   * Implement the XPath floor() function
   */
  def floor(): NumericValue = {
    new FloatValue(Math.floor(value).toFloat)
  }

  /**
   * Implement the XPath ceiling() function
   */
  def ceiling(): NumericValue = {
    new FloatValue(Math.ceil(value).toFloat)
  }

  /**
   * Implement the XPath round() function
   */
  def round(): NumericValue = {
    if (Float.isNaN(value)) {
      return this
    }
    if (Float.isInfinite(value)) {
      return this
    }
    if (value == 0.0) {
      return this
    }
    if (value >= -0.5 && value < 0.0) {
      return new FloatValue(-0.0.toFloat)
    }
    if (value > Integer.MIN_VALUE && value < Integer.MAX_VALUE) {
      return new FloatValue(Math.round(value).toFloat)
    }
    this
  }

  /**
   * Implement the XPath round-to-half-even() function
   */
  def roundHalfToEven(scale: Int): NumericValue = {
    new DoubleValue(value.toDouble).roundHalfToEven(scale)
      .convert(AtomicType.FLOAT)
      .asAtomic().asInstanceOf[FloatValue]
  }

  /**
   * Determine whether the value is negative, zero, or positive
   * @return -1 if negative, 0 if zero (including negative zero), +1 if positive, NaN if NaN
   */
  def signum(): Double = {
    if (Float.isNaN(value)) {
      return value
    }
    if (value > 0) return 1
    if (value == 0) return 0
    -1
  }

  /**
   * Determine whether the value is a whole number, that is, whether it compares
   * equal to some integer
   */
  def isWholeNumber(): Boolean = {
    value == Math.floor(value) && !Float.isInfinite(value)
  }

  /**
   * Get the absolute value as defined by the XPath abs() function
   * @return the absolute value
   * @since 9.2
   */
  def abs(): NumericValue = {
    if (value > 0.0) {
      this
    } else {
      new FloatValue(Math.abs(value))
    }
  }

  def compareTo(other: AnyRef): Int = {
    if (!other.isInstanceOf[NumericValue]) {
      throw new ClassCastException("Numeric values are not comparable to " + other.getClass)
    }
    if (other.isInstanceOf[FloatValue]) {
      val otherFloat = other.asInstanceOf[FloatValue].value
      if (value == otherFloat) return 0
      if (value < otherFloat) return -1
      return +1
    }
    if (other.isInstanceOf[DoubleValue]) {
      return super.compareTo(other)
    }
    compareTo(other.asInstanceOf[NumericValue].convert(AtomicType.FLOAT)
      .asAtomic())
  }

  /**
   * Compare the value to a long
   * @param other the value to be compared with
   * @return -1 if this is less, 0 if this is equal, +1 if this is greater or if this is NaN
   */
  def compareTo(other: Long): Int = {
    val otherFloat = other.toFloat
    if (value == otherFloat) return 0
    if (value < otherFloat) return -1
    +1
  }
}

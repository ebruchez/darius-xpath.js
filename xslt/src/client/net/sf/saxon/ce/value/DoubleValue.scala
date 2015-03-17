// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.ValidationFailure
import java.math.BigDecimal
import DoubleValue._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleValue {

  val ZERO = new DoubleValue(0.0)

  val NEGATIVE_ZERO = new DoubleValue(-0.0)

  val ONE = new DoubleValue(1.0)

  val NaN = new DoubleValue(Double.NaN)

  def isNegativeZero(d: Double): Boolean = {
    new java.lang.Double(d) == NEGATIVE_ZERO
  }

  /* native */ def convertToString(num: Double): String
}

/**
 * A numeric (double precision floating point) value
 */
class DoubleValue(var value: Double) extends NumericValue {

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.DOUBLE

  /**
   * Return this numeric value as a double
   *
   * @return the value as a double
   */
  def getDoubleValue(): Double = value

  /**
   * Get the hashCode. This must conform to the rules for other NumericValue hashcodes
   *
   * @see NumericValue#hashCode
   */
  override def hashCode(): Int = {
    if (value > Integer.MIN_VALUE && value < Integer.MAX_VALUE) {
      value.toInt
    } else {
      new java.lang.Double(value).hashCode
    }
  }

  /**
   * Test whether the value is the double/float value NaN
   */
  def isNaN(): Boolean = Double.isNaN(value)

  /**
   * Get the effective boolean value
   *
   * @return the effective boolean value (true unless the value is zero or NaN)
   */
  def effectiveBooleanValue(): Boolean = (value != 0.0 && !Double.isNaN(value))

  /**
   * Convert to target data type
   *
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.NUMERIC || 
      requiredType == AtomicType.DOUBLE) {
      this
    } else if (requiredType == AtomicType.BOOLEAN) {
      BooleanValue.get(effectiveBooleanValue())
    } else if (requiredType == AtomicType.INTEGER) {
      if (Double.isNaN(value)) {
        return new ValidationFailure("Cannot convert double NaN to an integer", "FOCA0002")
      }
      if (Double.isInfinite(value)) {
        return new ValidationFailure("Cannot convert double INF to an integer", "FOCA0002")
      }
      IntegerValue.decimalToInteger(new BigDecimal(value))
    } else if (requiredType == AtomicType.DECIMAL) {
      try {
        new DecimalValue(value)
      } catch {
        case e: XPathException => new ValidationFailure(e.getMessage)
      }
    } else if (requiredType == AtomicType.FLOAT) {
      new FloatValue(value.toFloat)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else {
      new ValidationFailure("Cannot convert double to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert the double to a string according to the XPath 2.0 rules
   *
   * @return the string value
   */
  def getPrimitiveStringValue(): CharSequence = {
    if (Double.isNaN(value)) {
      return "NaN"
    } else if (Double.isInfinite(value)) {
      return (if (value > 0) "INF" else "-INF")
    }
    val a = Math.abs(value)
    if (isWholeNumber && a < 1e6) {
      if (isNegativeZero(value)) {
        return "-0"
      }
      "" + value.toLong
    } else {
      if (a < 1e6) {
        if (a >= 1e-3) {
          Double toString value
        } else if (a >= 1e-6) {
          BigDecimal.valueOf(value).toPlainString()
        } else {
          val dec = BigDecimal.valueOf(value)
          dec.toString
        }
      } else if (a < 1e7) {
        convertToString(value)
      } else {
        Double toString value
      }
    }
  }

  /**
   * Negate the value
   */
  def negate(): NumericValue = new DoubleValue(-value)

  /**
   * Implement the XPath floor() function
   */
  def floor(): NumericValue = new DoubleValue(Math.floor(value))

  /**
   * Implement the XPath ceiling() function
   */
  def ceiling(): NumericValue = new DoubleValue(Math.ceil(value))

  /**
   * Implement the XPath round() function
   */
  def round(): NumericValue = {
    if (Double.isNaN(value)) {
      return this
    }
    if (Double.isInfinite(value)) {
      return this
    }
    if (value == 0.0) {
      return this
    }
    if (value >= -0.5 && value < 0.0) {
      return new DoubleValue(-0.0)
    }
    if (value > Long.MIN_VALUE && value < Long.MAX_VALUE) {
      return new DoubleValue(Math.round(value))
    }
    this
  }

  /**
   * Implement the XPath round-to-half-even() function
   */
  def roundHalfToEven(scale: Int): NumericValue = {
    if (Double.isNaN(value)) return this
    if (Double.isInfinite(value)) return this
    if (value == 0.0) return this
    val factor = Math.pow(10, scale + 1)
    var d = Math.abs(value * factor)
    if (Double.isInfinite(d)) {
      var dec = new BigDecimal(value)
      dec = dec.setScale(scale, BigDecimal.ROUND_HALF_EVEN)
      return new DoubleValue(dec.doubleValue())
    }
    val rem = d % 10
    if (rem > 5) {
      d += (10 - rem)
    } else if (rem < 5) {
      d -= rem
    } else {
      if ((d % 20) == 15) {
        d += 5
      } else {
        d -= 5
      }
    }
    d /= factor
    if (value < 0) {
      d = -d
    }
    new DoubleValue(d)
  }

  /**
   * Determine whether the value is negative, zero, or positive
   *
   * @return -1 if negative, 0 if zero (including negative zero), +1 if positive, NaN if NaN
   */
  def signum(): Double = {
    if (Double.isNaN(value)) {
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
    value == Math.floor(value) && !Double.isInfinite(value)
  }

  /**
   * Get the absolute value as defined by the XPath abs() function
   *
   * @return the absolute value
   * @since 9.2
   */
  def abs(): NumericValue = {
    if (value > 0.0) {
      this
    } else {
      new DoubleValue(Math.abs(value))
    }
  }

  /**
   * Compare the value to a long.
   *
   * @param other the value to be compared with
   * @return -1 if this is less, 0 if this is equal, +1 if this is greater or if this is NaN
   */
  def compareTo(other: Long): Int = {
    val otherDouble = other.toDouble
    if (value == otherDouble) return 0
    if (value < otherDouble) return -1
    +1
  }
}

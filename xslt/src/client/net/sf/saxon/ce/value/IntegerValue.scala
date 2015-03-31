// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.ValidationFailure
import java.math.BigDecimal
import java.math.BigInteger
import IntegerValue._
//remove if not needed
import scala.collection.JavaConversions._

object IntegerValue {

  /**
   * IntegerValue representing the value -1
   */
  val MINUS_ONE = new IntegerValue(-1)

  /**
   * IntegerValue representing the value zero
   */
  val ZERO = new IntegerValue(0)

  /**
   * IntegerValue representing the value +1
   */
  val PLUS_ONE = new IntegerValue(+1)

  /**
   * IntegerValue representing the maximum value for a long
   */
  val MAX_LONG = new IntegerValue(new BigDecimal(Long.MAX_VALUE))

  def decimalToInteger(value: BigDecimal): ConversionResult = {
    val setScaleValue = value.setScale(0, BigDecimal.ROUND_DOWN).intValue()
    new IntegerValue(setScaleValue)
  }

  /**
   * Static factory method to convert strings to integers.
   * @param s CharSequence representing the string to be converted
   * @return an IntegerValue representing the value of the String, or
   * a ValidationFailure encapsulating an Exception if the value cannot be converted.
   */
  def stringToInteger(s: CharSequence): ConversionResult = {
    try {
      var t = Whitespace.trimWhitespace(s).toString
      if (t.indexOf('.') >= 0) {
        t = "*"
      }
      new IntegerValue(new BigDecimal(t))
    } catch {
      case err: NumberFormatException ⇒ new ValidationFailure("Cannot convert string '" + s + "' to an integer",
        "FORG0001")
    }
  }

  /**
   * Get the signum of an int
   * @param i the int
   * @return -1 if the integer is negative, 0 if it is zero, +1 if it is positive
   */
  protected def signum(i: Int): Int = (i >> 31) | (-i >>> 31)
}

/**
 * This class represents the XPath built-in type xs:integer. It is used for all
 * subtypes of xs:integer, other than user-defined subtypes. Unlike other Saxon editions,
 * IntegerValue is implemented as a subclass of DecimalValue. This is because there is
 * no point in the optimisation whereby small integers are mapped to long, since GWT
 * emulates long using two doubles.
 */
class IntegerValue(value: Int) extends DecimalValue(value) {

  def this(value: BigDecimal) {
    super(value)
    if (value.scale() != 0 && 
      value.compareTo(value.setScale(0, BigDecimal.ROUND_DOWN)) != 
      0) {
      throw new IllegalArgumentException("Non-integral value " + value)
    }
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType: AtomicType = AtomicType.INTEGER

  /**
   * Determine whether the value is a whole number, that is, whether it compares
   * equal to some integer
   *
   * @return always true for this implementation
   */
  def isWholeNumber: Boolean = true

  /**
   * Take modulo another integer
   * @param other the other integer
   * @return the result of the modulo operation (the remainder)
   * @throws XPathException if the other integer is zero
   */
  def mod(other: IntegerValue): IntegerValue = {
    try {
      new IntegerValue(getDecimalValue.remainder(other.getDecimalValue))
    } catch {
      case err: ArithmeticException ⇒ {
        var e: XPathException = null
        e = if (BigInteger.valueOf(other.intValue()).signum() == 0) new XPathException("Integer modulo zero", 
          "FOAR0001") else new XPathException("Integer mod operation failure", err)
        throw e
      }
    }
  }

  override def intValue(): Int = {
    if (getDecimalValue.compareTo(BIG_DECIMAL_MIN_INT) < 0 || getDecimalValue.compareTo(BIG_DECIMAL_MAX_INT) > 0) {
      throw new XPathException("int out of range")
    } else {
      getDecimalValue.intValue()
    }
  }

  /**
   * Get the absolute value as defined by the XPath abs() function
   * @return the absolute value
   * @since 9.2
   */
  def abs(): NumericValue = {
    if (getDecimalValue.signum() > 0) {
      this
    } else {
      negate()
    }
  }

  /**
   * Negate the value
   */
  def negate(): NumericValue = {
    new IntegerValue(getDecimalValue.negate())
  }

  /**
   * Implement the XPath floor() function
   */
  def floor(): NumericValue = this

  /**
   * Implement the XPath ceiling() function
   */
  def ceiling(): NumericValue = this
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import java.math.{BigDecimal, BigInteger}

import client.net.sf.saxon.ce.`type`.{AtomicType, ConversionResult, ValidationFailure}
import client.net.sf.saxon.ce.regex.RegExp
import client.net.sf.saxon.ce.trans.{Err, XPathException}
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.DecimalValue._

import scala.util.control.Breaks

object DecimalValue {

  val DIVIDE_PRECISION = 18
  val BIG_DECIMAL_ONE_MILLION = BigDecimal.valueOf(1000000)
  val BIG_DECIMAL_MAX_INT = BigDecimal.valueOf(Integer.MAX_VALUE)
  val BIG_DECIMAL_MIN_INT = BigDecimal.valueOf(Integer.MIN_VALUE)
  val ZERO = new DecimalValue(BigDecimal.valueOf(0))
  val ONE = new DecimalValue(BigDecimal.valueOf(1))
  val TWO = new DecimalValue(BigDecimal.valueOf(2))

  /**
   * @param value a BigDecimal that may have trailing zeros
   * @return a value that has trailing zeros trimmed
   *         Used instead of BigDecimal's own method as this has a
   *         GWT bug 6110 reported causing infinite recursion: see http://code.google.com/p/google-web-toolkit/issues/detail?id=6110
   */
  def stripTrailingZeros(value: BigDecimal): BigDecimal = {
    val str = value.toString
    val dotPos = str.indexOf('.')
    if (dotPos < 0) {
      return value
    }
    val expPos = str.indexOf('E')
    if (expPos > -1) {
      return value
    }
    var lastZeroPos = -1
    val zeroCh = '0'
    var i = str.length - 1
    import Breaks._
    breakable {
      while (i > -1) {
        val ch = str.charAt(i)
        if (ch != zeroCh)
          break()
        lastZeroPos = i
        i -= 1
      }
    }
    if (lastZeroPos > -1) {
      if (lastZeroPos - dotPos == 1) lastZeroPos = dotPos
      val strippedValue = str.substring(0, lastZeroPos)
      new BigDecimal(strippedValue)
    } else {
      value
    }
  }

  private val decimalPattern = RegExp.compile("(\\-|\\+)?((\\.[0-9]+)|([0-9]+(\\.[0-9]*)?))")

  /**
   * Factory method to construct a DecimalValue from a string
   *
   * @param in the value of the DecimalValue
   * @return the required DecimalValue if the input is valid, or a ValidationFailure encapsulating the error
   *         message if not.
   */
  def makeDecimalValue(in: CharSequence): ConversionResult = {
    try {
      val digits = new FastStringBuffer(in.length)
      var scale = 0
      var state = 0
      var foundDigit = false
      val len = in.length
      for (i ← 0 until len) {
        val c = in.charAt(i)
        c match {
          case ' ' | '\t' | '\r' | '\n' ⇒ if (state != 0) {
            state = 5
          }
          case '+' ⇒
            if (state != 0) {
              throw new NumberFormatException("unexpected sign")
            }
            state = 1

          case '-' ⇒
            if (state != 0) {
              throw new NumberFormatException("unexpected sign")
            }
            state = 1
            digits.append(c)

          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
            if (state == 0) {
              state = 1
            } else if (state >= 3) {
              scale += 1
            }
            if (state == 5) {
              throw new NumberFormatException("contains embedded whitespace")
            }
            digits.append(c)
            foundDigit = true

          case '.' ⇒
            if (state == 5) {
              throw new NumberFormatException("contains embedded whitespace")
            }
            if (state >= 3) {
              throw new NumberFormatException("more than one decimal point")
            }
            state = 3

          case _ ⇒ throw new NumberFormatException("invalid character '" + c + "'")
        }
      }
      if (!foundDigit) {
        throw new NumberFormatException("no digits in value")
      }
      import Breaks._
      breakable {
        while (scale > 0) {
          if (digits.charAt(digits.length - 1) == '0') {
            digits.setLength(digits.length - 1)
            scale -= 1
          } else {
            break()
          }
        }
      }
      if (digits.length == 0 || (digits.length == 1 && digits.charAt(0) == '-')) {
        return DecimalValue.ZERO
      }
      val bigInt = new BigInteger(digits.toString)
      val bigDec = new BigDecimal(bigInt, scale)
      new DecimalValue(bigDec)
    } catch {
      case err: NumberFormatException ⇒ new ValidationFailure("Cannot convert string " + Err.wrap(Whitespace.trim(in),
        Err.VALUE) + 
        " to xs:decimal: " + 
        err.getMessage, "FORG0001")
    }
  }

  /**
   * Test whether a string is castable to a decimal value
   *
   * @param in the string to be tested
   * @return true if the string has the correct format for a decimal
   */
  def castableAsDecimal(in: CharSequence): Boolean = {
    val trimmed = Whitespace.trimWhitespace(in)
    decimalPattern.exec(trimmed.toString) != null
  }

  /**
   * Convert a decimal value to a string, using the XPath rules for formatting
   *
   * @param value the decimal value to be converted
   * @param fsb   the FastStringBuffer to which the value is to be appended
   * @return the supplied FastStringBuffer, suitably populated
   */
  def decimalToString(value: BigDecimal, fsb: FastStringBuffer): FastStringBuffer = {
    val scale = value.scale()
    if (scale == 0) {
      fsb.append(value.toString)
      fsb
    } else if (scale < 0) {
      val s = value.abs().unscaledValue().toString
      if (s == "0") {
        fsb.append('0')
        return fsb
      }
      if (value.signum() < 0) {
        fsb.append('-')
      }
      fsb.append(s)
      for (i ← 0 until (-scale)) {
        fsb.append('0')
      }
      fsb
    } else {
      val s = value.abs().unscaledValue().toString
      if (s == "0") {
        fsb.append('0')
        return fsb
      }
      val len = s.length
      if (value.signum() < 0) {
        fsb.append('-')
      }
      if (scale >= len) {
        fsb.append("0.")
        for (i ← len until scale) {
          fsb.append('0')
        }
        fsb.append(s)
      } else {
        fsb.append(s.substring(0, len - scale))
        fsb.append('.')
        fsb.append(s.substring(len - scale))
      }
      fsb
    }
  }
}

/**
 * A decimal value
 */
class DecimalValue(_value: Either[BigDecimal, Double]) extends NumericValue {

  private val value = _value match {
    case Left(decimal) ⇒ stripTrailingZeros(decimal)
    case Right(double) ⇒ new BigDecimal(double)
  }

  def this(in: BigDecimal) =
    this(Left(in))

  /**
   * Constructor supplying a double
   *
   * @param in the value of the DecimalValue
   * @throws XPathException if the double cannot be converted (e.g. Infinity or NaN)
   */
  def this(in: Double) =
    this(Right(in))

  /**
   * Constructor supplying a long integer
   *
   * @param in the value of the DecimalValue
   */
  def this(in: Long) =
    this(BigDecimal.valueOf(in))

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.DECIMAL

  /**
   * Get the value
   */
  override def getDecimalValue(): BigDecimal = value

  /**
   * Get the hashCode. This must conform to the rules for other NumericValue hashcodes
   *
   * @see NumericValue#hashCode
   */
  override def hashCode(): Int = {
    val round = value.setScale(0, BigDecimal.ROUND_DOWN)
    val result = round.longValue()
    if (result > Integer.MIN_VALUE && result < Integer.MAX_VALUE) {
      result.toInt
    } else {
      new java.lang.Double(getDoubleValue).hashCode
    }
  }

  override def effectiveBooleanValue(): Boolean = value.signum() != 0

  /**
   * Convert to target data type
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.NUMERIC || 
      requiredType == AtomicType.DECIMAL) {
      this
    } else if (requiredType == AtomicType.INTEGER) {
      IntegerValue.decimalToInteger(value)
    } else if (requiredType == AtomicType.BOOLEAN) {
      BooleanValue.get(value.signum() != 0)
    } else if (requiredType == AtomicType.DOUBLE) {
      new DoubleValue(value.doubleValue())
    } else if (requiredType == AtomicType.FLOAT) {
      new FloatValue(value.floatValue())
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else {
      new ValidationFailure("Cannot convert decimal to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Get the value as a String
   *
   * @return a String representation of the value
   */
  def getPrimitiveStringValue(): CharSequence = {
    decimalToString(value, new FastStringBuffer(FastStringBuffer.TINY))
  }

  /**
   * Negate the value
   */
  def negate(): NumericValue = new DecimalValue(value.negate())

  /**
   * Implement the XPath floor() function
   */
  def floor(): NumericValue = {
    new DecimalValue(value.setScale(0, BigDecimal.ROUND_FLOOR))
  }

  /**
   * Implement the XPath ceiling() function
   */
  def ceiling(): NumericValue = {
    new DecimalValue(value.setScale(0, BigDecimal.ROUND_CEILING))
  }

  /**
   * Implement the XPath round() function
   */
  def round(): NumericValue = value.signum() match {
    case -1 ⇒ new DecimalValue(value.setScale(0, BigDecimal.ROUND_HALF_DOWN))
    case 0 ⇒ this
    case 1 ⇒ new DecimalValue(value.setScale(0, BigDecimal.ROUND_HALF_UP))
    case _ ⇒ this
  }

  /**
   * Implement the XPath round-half-to-even() function
   */
  def roundHalfToEven(scale: Int): NumericValue = {
    val scaledValue = value.setScale(scale, BigDecimal.ROUND_HALF_EVEN)
    new DecimalValue(scaledValue)
  }

  /**
   * Determine whether the value is negative, zero, or positive
   *
   * @return -1 if negative, 0 if zero, +1 if positive, NaN if NaN
   */
  def signum(): Double = value.signum()

  /**
   * Determine whether the value is a whole number, that is, whether it compares
   * equal to some integer
   */
  def isWholeNumber(): Boolean = {
    value.scale() == 0 || 
      value.compareTo(value.setScale(0, BigDecimal.ROUND_DOWN)) == 
      0
  }

  /**
   * Get the absolute value as defined by the XPath abs() function
   *
   * @return the absolute value
   * @since 9.2
   */
  def abs(): NumericValue = {
    if (value.signum() > 0) {
      this
    } else {
      new DecimalValue(value.negate())
    }
  }

  /**
   * Compare the value to another numeric value
   */
  override def compareTo(other: AnyRef): Int = {
    if (other.isInstanceOf[DecimalValue]) {
      value.compareTo(other.asInstanceOf[DecimalValue].value)
    } else if (other.isInstanceOf[FloatValue]) {
      val f = convert(AtomicType.FLOAT).asAtomic().asInstanceOf[FloatValue]
      f.compareTo(other)
    } else {
      super.compareTo(other)
    }
  }

  /**
   * Compare the value to a long
   *
   * @param other the value to be compared with
   * @return -1 if this is less, 0 if this is equal, +1 if this is greater or if this is NaN
   */
  def compareTo(other: Long): Int = {
    if (other == 0) {
      return value.signum()
    }
    value.compareTo(BigDecimal.valueOf(other))
  }
}

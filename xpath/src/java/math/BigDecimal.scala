/*
*
*  Ported by Alistair Johnson from  https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/BigDecimal.java
*/

package java.math

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Arrays

import client.net.sf.saxon.ce.orbeon.Util

/**
 * Companion object for {@code BigDecimal}
 */
object BigDecimal {

  /**
   * Rounding mode where positive values are rounded towards positive infinity
   * and negative values towards negative infinity.
   *
   * @see RoundingMode#UP
   */
  val ROUND_UP = 0

  /**
   * Rounding mode where the values are rounded towards zero.
   *
   * @see RoundingMode#DOWN
   */
  val ROUND_DOWN = 1

  /**
   * Rounding mode to round towards positive infinity. For positive values
   * this rounding mode behaves as {@link #ROUND_UP}, for negative values as
   * {@link #ROUND_DOWN}.
   *
   * @see RoundingMode#CEILING
   */
  val ROUND_CEILING = 2

  /**
   * Rounding mode to round towards negative infinity. For positive values
   * this rounding mode behaves as {@link #ROUND_DOWN}, for negative values as
   * {@link #ROUND_UP}.
   *
   * @see RoundingMode#FLOOR
   */
  val ROUND_FLOOR = 3

  /**
   * Rounding mode where values are rounded towards the nearest neighbor.
   * Ties are broken by rounding up.
   *
   * @see RoundingMode#HALF_UP
   */
  val ROUND_HALF_UP = 4

  /**
   * Rounding mode where values are rounded towards the nearest neighbor.
   * Ties are broken by rounding down.
   *
   * @see RoundingMode#HALF_DOWN
   */
  val ROUND_HALF_DOWN = 5

  /**
   * Rounding mode where values are rounded towards the nearest neighbor.
   * Ties are broken by rounding to the even neighbor.
   *
   * @see RoundingMode#HALF_EVEN
   */
  val ROUND_HALF_EVEN = 6

  /**
   * Rounding mode where the rounding operations throws an {@code
   * ArithmeticException} for the case that rounding is necessary, i.e. for
   * the case that the value cannot be represented exactly.
   *
   * @see RoundingMode#UNNECESSARY
   */
  val ROUND_UNNECESSARY = 7

  /** The double closest to {@code Log10(2)}. */
  private val LOG10_2 = 0.3010299956639812

  /**
   * An array with powers of five that fit in the type <code>long</code>
   * (<code>5^0,5^1,...,5^27</code>).
   */
  private val FIVE_POW = Multiplication.bigFivePows

  /**
   * An array with powers of ten that fit in the type <code>long</code>
   * (<code>10^0,10^1,...,10^18</code>).
   */
  private val TEN_POW = Multiplication.bigTenPows

  private val LONG_FIVE_POW = Array[Long](
    1L,
    5L,
    25L,
    125L,
    625L,
    3125L,
    15625L,
    78125L,
    390625L,
    1953125L,
    9765625L,
    48828125L,
    244140625L,
    1220703125L,
    6103515625L,
    30517578125L,
    152587890625L,
    762939453125L,
    3814697265625L,
    19073486328125L,
    95367431640625L,
    476837158203125L,
    2384185791015625L,
    11920928955078125L,
    59604644775390625L,
    298023223876953125L,
    1490116119384765625L,
    7450580596923828125L
  )

  private val LONG_FIVE_POW_BIT_LENGTH = new Array[Int](LONG_FIVE_POW.length)

  /**
   * An array with powers of ten that fit in the type <code>long</code>
   * (<code>10^0,10^1,...,10^18</code>).
   */
  private val  LONG_POWERS_OF_TEN = Array[Long] (
    1L,
    10L,
    100L,
    1000L,
    10000L,
    100000L,
    1000000L,
    10000000L,
    100000000L,
    1000000000L,
    10000000000L,
    100000000000L,
    1000000000000L,
    10000000000000L,
    100000000000000L,
    1000000000000000L,
    10000000000000000L,
    100000000000000000L,
    1000000000000000000L
  )

  private val LONG_POWERS_OF_TEN_BIT_LENGTH = new Array[Int](LONG_POWERS_OF_TEN.length)

  private val BI_SCALED_BY_ZERO_LENGTH = 11

  /**
   * An array with the first <code>BigInteger</code> scaled by zero.
   * (<code>[0,0],[1,0],...,[10,0]</code>).
   */
  private val BI_SCALED_BY_ZERO = new Array[BigDecimal](BI_SCALED_BY_ZERO_LENGTH)

  /**
   * An array with the zero number scaled by the first positive scales.
   * (<code>0*10^0, 0*10^1, ..., 0*10^10</code>).
   */
  private val ZERO_SCALED_BY = new Array[BigDecimal](11)

  /** An array filled with characters <code>'0'</code>. */
  private val CH_ZEROS = new Array[Char](100)

  Arrays.fill(CH_ZEROS, '0')

  for (i ← 0 until ZERO_SCALED_BY.length) {
    BI_SCALED_BY_ZERO(i) = new BigDecimal(i, 0)
    ZERO_SCALED_BY(i) = new BigDecimal(0, i)
  }

  for (i ← 0 until LONG_FIVE_POW_BIT_LENGTH.length) {
    LONG_FIVE_POW_BIT_LENGTH(i) = bitLength(LONG_FIVE_POW(i))
  }

  for (i ← 0 until LONG_POWERS_OF_TEN_BIT_LENGTH.length) {
    LONG_POWERS_OF_TEN_BIT_LENGTH(i) = bitLength(LONG_POWERS_OF_TEN(i))
  }

  /**
   * The constant zero as a {@code BigDecimal}.
   */
  val ZERO = new BigDecimal(0, 0)

  /**
   * The constant one as a {@code BigDecimal}.
   */
  val ONE = new BigDecimal(1, 0)

  /**
   * The constant ten as a {@code BigDecimal}.
   */
  val TEN = new BigDecimal(10, 0)

  /**
   * Returns a new {@code BigDecimal} instance whose value is equal to {@code
   * unscaledVal * 10<sup>-scale</sup>}). The scale of the result is {@code
   * scale}, and its unscaled value is {@code unscaledVal}.
   */
  def valueOf(unscaledVal: Long, scale: Int): BigDecimal = {
    if (scale == 0) {
      valueOf(unscaledVal)
    }
    else if ((unscaledVal == 0) && (scale >= 0) && (scale < ZERO_SCALED_BY.length)) {
      ZERO_SCALED_BY(scale)
    }else
    new BigDecimal(unscaledVal, scale)
  }

  /**
   * Returns a new {@code BigDecimal} instance whose value is equal to {@code
   * unscaledVal}. The scale of the result is {@code 0}, and its unscaled
   * value is {@code unscaledVal}.
   *
   * @param unscaledVal
     *            value to be converted to a {@code BigDecimal}.
   * @return {@code BigDecimal} instance with the value {@code unscaledVal}.
   */
  def valueOf(unscaledVal: Long): BigDecimal = {
    if ((unscaledVal >= 0) && (unscaledVal < BI_SCALED_BY_ZERO_LENGTH)) {
      return BI_SCALED_BY_ZERO(unscaledVal.toInt)
    }
    new BigDecimal(unscaledVal, 0)
  }

  /**
   * Returns a new {@code BigDecimal} instance whose value is equal to {@code
   * val}. The new decimal is constructed as if the {@code BigDecimal(String)}
   * constructor is called with an argument which is equal to {@code
   * Double.toString(val)}. For example, {@code valueOf("0.1")} is converted to
   * (unscaled=1, scale=1), although the double {@code 0.1} cannot be
   * represented exactly as a double value. In contrast to that, a new {@code
   * BigDecimal(0.1)} instance has the value {@code
   * 0.1000000000000000055511151231257827021181583404541015625} with an
   * unscaled value {@code 1000000000000000055511151231257827021181583404541015625}
   * and the scale {@code 55}.
   *
   * @param val
     *            double value to be converted to a {@code BigDecimal}.
   * @return {@code BigDecimal} instance with the value {@code val}.
   * @throws NumberFormatException
     *             if {@code val} is infinite or {@code val} is not a number
   */
  def valueOf(d: Double): BigDecimal = {
    if (java.lang.Double.isInfinite(d) || java.lang.Double.isNaN(d)) {
      throw new NumberFormatException("Infinity or NaN: " + d)
    }
    new BigDecimal(java.lang.Double.toString(d))
  }

  private def addAndMult10(thisValue: BigDecimal, augend: BigDecimal, diffScale: Int): BigDecimal = {
    if (diffScale < LONG_POWERS_OF_TEN.length &&
      Math.max(thisValue._bitLength, augend._bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(diffScale)) + 1 < 64) {
      valueOf(thisValue._smallValue + augend._smallValue * LONG_POWERS_OF_TEN(diffScale), thisValue._scale)
    } else {
      new BigDecimal(thisValue.getUnscaledValue().add(
        Multiplication.multiplyByTenPow(augend.getUnscaledValue(), diffScale)), thisValue.scale)
    }
  }

  private def divideBigIntegers(scaledDividend: BigInteger,
                                scaledDivisor: BigInteger,
                                scale: Int,
                                roundingMode: RoundingMode): BigDecimal = {
    val quotAndRem = scaledDividend.divideAndRemainder(scaledDivisor) // quotient and remainder
    // If after division there is a remainder...
    var quotient = quotAndRem(0)
    val remainder = quotAndRem(1)
    if (remainder.signum() == 0) {
      return new BigDecimal(quotient, scale)
    }
    val sign = scaledDividend.signum() * scaledDivisor.signum()
    var compRem: Int = 0 // 'compare to remainder'
    if (scaledDivisor.bitLength() < 63) { // 63 in order to avoid out of long after *2
      val rem = remainder.longValue()
      val divisor = scaledDivisor.longValue()
      compRem = longCompareTo(Math.abs(rem) * 2, Math.abs(divisor))
      // To look if there is a carry
      compRem = roundingBehavior(if (quotient.testBit(0)) 1 else 0, sign * (5 + compRem), roundingMode)
    } else {
      // Checking if:  remainder * 2 >= scaledDivisor
      compRem = remainder.abs().shiftLeftOneBit().compareTo(scaledDivisor.abs())
      compRem = roundingBehavior(if (quotient.testBit(0)) 1 else 0, sign * (5 + compRem), roundingMode)
    }
    if (compRem != 0) {
      if (quotient.bitLength() < 63) {
        return valueOf(quotient.longValue() + compRem, scale)
      }
      quotient = quotient.add(BigInteger.valueOf(compRem))
      return new BigDecimal(quotient, scale)
    }
    // Constructing the result with the appropriate unscaled value
    new BigDecimal(quotient, scale)
  }

  private def dividePrimitiveLongs(scaledDividend: Long,
                                   scaledDivisor: Long,
                                   scale: Int,
                                   roundingMode: RoundingMode): BigDecimal = {
    var quotient = scaledDividend / scaledDivisor
    val remainder = scaledDividend % scaledDivisor
    val sign = Util.signum(scaledDividend) * Util.signum(scaledDivisor)
    if (remainder != 0) {
      // Checking if:  remainder * 2 >= scaledDivisor
      var compRem: Int = 0
      compRem = longCompareTo(Math.abs(remainder) * 2, Math.abs(scaledDivisor))
      // To look if there is a carry
      quotient += roundingBehavior(quotient.toInt & 1, sign * (5 + compRem), roundingMode)
    }
    // Constructing the result with the appropriate unscaled value
    valueOf(quotient, scale)
  }

  private def longCompareTo(value1: Long, value2: Long): Int = {
    if (value1 > value2) 1 else if (value1 < value2) -1 else 0
  }

  /**
   * Return an increment that can be -1,0 or 1, depending of
   * {@code roundingMode}.
   *
   * @param parityBit
     *            can be 0 or 1, it's only used in the case
   *            {@code HALF_EVEN}
   * @param fraction
     *            the mantissa to be analyzed
   * @param roundingMode
     *            the type of rounding
   * @return the carry propagated after rounding
   */
  private def roundingBehavior(parityBit: Int, fraction: Int, roundingMode: RoundingMode): Int = {
    var increment = 0 // the carry after rounding
    roundingMode match {
      case RoundingMode.UNNECESSARY ⇒ if (fraction != 0) {
        throw new ArithmeticException("Rounding necessary")
      }
      case RoundingMode.UP ⇒ increment = java.lang.Integer.signum(fraction)
      case RoundingMode.DOWN ⇒ //break in case statement
      case RoundingMode.CEILING ⇒ increment = Math.max(java.lang.Integer.signum(fraction), 0)
      case RoundingMode.FLOOR ⇒ increment = Math.min(java.lang.Integer.signum(fraction), 0)
      case RoundingMode.HALF_UP ⇒ if (Math.abs(fraction) >= 5) {
        increment = java.lang.Integer.signum(fraction)
      }
      case RoundingMode.HALF_DOWN ⇒ if (Math.abs(fraction) > 5) {
        increment = java.lang.Integer.signum(fraction)
      }
      case RoundingMode.HALF_EVEN ⇒ if (Math.abs(fraction) + parityBit > 5) {
        increment = java.lang.Integer.signum(fraction)
      }
    }
    increment
  }

  private def safeLongToInt(longValue: Long): Int = {
    if (longValue < java.lang.Integer.MIN_VALUE || longValue > java.lang.Integer.MAX_VALUE) {
      throw new ArithmeticException("Out of int range: " + longValue)
    }
    longValue.toInt
  }

  /**
   * It returns the value 0 with the most approximated scale of type
   * {@code int}. if {@code longScale > Integer.MAX_VALUE} the
   * scale will be {@code Integer.MAX_VALUE}; if
   * {@code longScale < Integer.MIN_VALUE} the scale will be
   * {@code Integer.MIN_VALUE}; otherwise {@code longScale} is
   * casted to the type {@code int}.
   *
   * @param longScale
     *            the scale to which the value 0 will be scaled.
   * @return the value 0 scaled by the closer scale of type {@code int}.
   * @see #scale
   */
  private def zeroScaledBy(longScale: Long): BigDecimal = {
    if (longScale == longScale.toInt) {
      return valueOf(0, longScale.toInt)
    }
    if (longScale >= 0) {
      return new BigDecimal(0, java.lang.Integer.MAX_VALUE)
    }
    new BigDecimal(0, java.lang.Integer.MIN_VALUE)
  }

  protected def bitLength( sValue: Long): Int = {
    val smallValue = if (sValue < 0) ~sValue else sValue
    64 - java.lang.Long.numberOfLeadingZeros(smallValue)
  }

  private def bitLength(sValue: Int): Int = {
    val smallValue = if (sValue < 0) ~sValue else sValue
    32 - java.lang.Integer.numberOfLeadingZeros(smallValue)
  }
}

/**
 * An immutable arbitrary-precision signed decimal.
 *
 * <p>A value is represented by an arbitrary-precision "unscaled value" and a signed 32-bit "scale",
 * combined thus: {@code unscaled * 10<sup>-scale</sup>}. See {@link #unscaledValue} and {@link #scale}.
 *
 * <p>Most operations allow you to supply a {@link MathContext} to specify a desired rounding mode.
 */
class BigDecimal() extends Number with Comparable[BigDecimal] with Serializable {


  import BigDecimal._

  /** The <code>String</code> representation is cached. */
  @transient private var toStringImage: String = null

  /** Cache for the hash code. */
  @transient private var _hashCode: Int = 0

  /**
   * The arbitrary precision integer (unscaled value) in the internal
   * representation of {@code BigDecimal}.
   */
  private var intVal: BigInteger = _

  @transient private var _bitLength: Int = 0

  @transient private var _smallValue: Long = 0

  /**
   * The 32-bit integer scale in the internal representation of {@code BigDecimal}.
   */
  private    var _scale: Int = 0

  /**
   * Represent the number of decimal digits in the unscaled value. This
   * precision is calculated the first time, and used in the following calls
   * of method <code>precision()</code>. Note that some call to the private
   * method <code>inplaceRound()</code> could update this field.
   *
   * @see #precision()
   * @see #inplaceRound(MathContext)
   */
  @transient private var _precision: Int = 0

  private def this(smallValue: Long, scale: Int) {
    this()
    this._smallValue = smallValue
    this._scale = scale
    this._bitLength = bitLength(smallValue)
  }

  private def this(smallValue: Int, scale: Int) {
    this()
    this._smallValue = smallValue
    this._scale = scale
    this._bitLength = bitLength(smallValue)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a string representation
   * given as a character array.
   *
   * @param in
   *            array of characters containing the string representation of
   *            this {@code BigDecimal}.
   * @param offset
   *            first index to be copied.
   * @param len
   *            number of characters to be used.
   * @throws NumberFormatException
   *             if {@code offset < 0 || len <= 0 || offset+len-1 < 0 ||
   *             offset+len-1 >= in.length}, or if {@code in} does not
   *             contain a valid string representation of a big decimal.
   */
  def this(in: Array[Char], offset: Int, len: Int) {
    this()
    var _offset = offset
    var begin = _offset   // first index to be copied
    val last = _offset + (len - 1) // last index to be copied

    if (in == null) {
      throw new NullPointerException("in == null")
    }
    if ((last >= in.length) || (_offset < 0) || (len <= 0) || (last < 0)) {
      throw new NumberFormatException("Bad offset/length: offset=" + _offset + " len=" + len +
        " in.length=" +
        in.length)
    }
    val unscaledBuffer: java.lang.StringBuilder = new java.lang.StringBuilder("") //len)
    var bufLength = 0
    // To skip a possible '+' symbol
    if ((_offset <= last) && (in(_offset) == '+')) {
      _offset += 1
      begin += 1

      // Fail if the next character is another sign.
      if ((_offset < last) && (in(_offset) == '+' || in(_offset) == '-')) {
        throw new NumberFormatException("For input string: " + in.toString)
      }
    }else {
    // check that '-' is not folloed by another sign
      if ((_offset <= last) && (in(_offset) == '-')) {

        // Fail if the next character is another sign.
        if ((_offset + 1 < last) && (in(_offset+ 1) == '+' || in(_offset+ 1) == '-')) {
          throw new NumberFormatException("For input string: " + in.toString)
        }
      }
    }

    var counter = 0
    var wasNonZero = false
    // Accumulating all digits until a possible decimal point
    while ((_offset <= last) && (in(_offset) != '.') && (in(_offset) != 'e') &&
      (in(_offset) != 'E')) {
      if (!wasNonZero) {
        if (in(_offset) == '0') {
          counter += 1
        } else {
          wasNonZero = true
        }
      }
      _offset += 1
    }

    unscaledBuffer.append(in, begin, _offset - begin)
    bufLength += _offset - begin
    // A decimal point was found
    if ((_offset <= last) && (in(_offset) == '.')) {
      _offset += 1
      // Accumulating all digits until a possible exponent
      begin = _offset
      while ((_offset <= last) && (in(_offset) != 'e') && (in(_offset) != 'E')) {
        if (!wasNonZero) {
          if (in(_offset) == '0') {
            counter += 1
          } else {
            wasNonZero = true
          }
        }
        _offset += 1
      }
      _scale = _offset - begin
      bufLength += _scale
      unscaledBuffer.append(in, begin, _scale)
    } else {
      _scale = 0
    }
    // An exponent was found
    if ((_offset <= last) && ((in(_offset) == 'e') || (in(_offset) == 'E'))) {
      _offset += 1
      // Checking for a possible sign of scale
      begin = _offset
      if ((_offset <= last) && (in(_offset) == '+')) {
        _offset += 1
        if ((_offset <= last) && (in(_offset) != '-')) {
          begin += 1
        }
      }
      // Accumulating all remaining digits
      val scaleString = String.valueOf(in, begin, last + 1 - begin)
      // Checking if the scale is defined
      val newScale: Long = _scale.toLong - java.lang.Integer.parseInt(scaleString)
      _scale = newScale.toInt
      if (newScale != _scale) {
        throw new NumberFormatException("Scale out of range")
      }
    }
    // Parsing the unscaled value
    if (bufLength < 19) {
      _smallValue = java.lang.Long.parseLong(unscaledBuffer.toString)
      _bitLength = bitLength(_smallValue)
    } else {
      setUnscaledValue(new BigInteger(unscaledBuffer.toString))
    }
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a string representation
   * given as a character array.
   *
   * @param in
   *            array of characters containing the string representation of
   *            this {@code BigDecimal}.
   * @param offset
   *            first index to be copied.
   * @param len
   *            number of characters to be used.
   * @param mc
   *            rounding mode and precision for the result of this operation.
   * @throws NumberFormatException
   *             if {@code offset < 0 || len <= 0 || offset+len-1 < 0 ||
   *             offset+len-1 >= in.length}, or if {@code in} does not
   *             contain a valid string representation of a big decimal.
   * @throws ArithmeticException
   *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
   *             UNNECESSARY} and the new big decimal cannot be represented
   *             within the given precision without rounding.
   */
  def this(in: Array[Char],
           offset: Int,
           len: Int,
           mc: MathContext) {
    this(in, offset, len)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a string representation
   * given as a character array.
   *
   * @param in
   *            array of characters containing the string representation of
   *            this {@code BigDecimal}.
   * @throws NumberFormatException
   *             if {@code in} does not contain a valid string representation
   *             of a big decimal.
   */
  def this(in: Array[Char]) {
    this(in, 0, in.length)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a string representation
   * given as a character array. The result is rounded according to the
   * specified math context.
   *
   * @param in
   *            array of characters containing the string representation of
   *            this {@code BigDecimal}.
   * @param mc
   *            rounding mode and precision for the result of this operation.
   * @throws NumberFormatException
   *             if {@code in} does not contain a valid string representation
   *             of a big decimal.
   * @throws ArithmeticException
   *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
   *             UNNECESSARY} and the new big decimal cannot be represented
   *             within the given precision without rounding.
   */
  def this(in: Array[Char], mc: MathContext) {
    this(in, 0, in.length)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a string
   * representation.
   *
   * @throws NumberFormatException
   *             if {@code val} does not contain a valid string representation
   *             of a big decimal.
   */
  def this(sVal: String) {
    this(sVal.toCharArray, 0, sVal.length)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a string
   * representation. The result is rounded according to the specified math
   * context.
   *
   * @param mc
   *            rounding mode and precision for the result of this operation.
   * @throws NumberFormatException
   *             if {@code val} does not contain a valid string representation
   *             of a big decimal.
   * @throws ArithmeticException
   *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
   *             UNNECESSARY} and the new big decimal cannot be represented
   *             within the given precision without rounding.
   */
  def this(sVal: String, mc: MathContext) {
    this(sVal.toCharArray, 0, sVal.length)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the 64bit double
   * {@code val}. The constructed big decimal is equivalent to the given
   * double. For example, {@code new BigDecimal(0.1)} is equal to {@code
   * 0.1000000000000000055511151231257827021181583404541015625}. This happens
   * as {@code 0.1} cannot be represented exactly in binary.
   * <p>
   * To generate a big decimal instance which is equivalent to {@code 0.1} use
   * the {@code BigDecimal(String)} constructor.
   *
   * @param dVal
   *            double value to be converted to a {@code BigDecimal} instance.
   * @throws NumberFormatException
   *             if {@code val} is infinity or not a number.
   */
  def this(dVal: Double) {
    this()
    if (java.lang.Double.isInfinite(dVal) || java.lang.Double.isNaN(dVal)) {
      throw new NumberFormatException("Infinity or NaN: " + dVal)
    }
    val bits = java.lang.Double.doubleToLongBits(dVal)
    var mantissa: Long = 0l
    var trailingZeros: Int = 0
    // Extracting the exponent, note that the bias is 1023
    _scale = 1075 - ((bits >> 52) & 0x7FFL).toInt
    // Extracting the 52 bits of the mantissa.
    mantissa = if (_scale == 1075) (bits & 0xFFFFFFFFFFFFFL) << 1 else (bits & 0xFFFFFFFFFFFFFL) | 0x10000000000000L
    if (mantissa == 0) {
      _scale = 0
      _precision = 1
    }
    // To simplify all factors '2' in the mantissa
    if (_scale > 0) {
      trailingZeros = Math.min(_scale, java.lang.Long.numberOfTrailingZeros(mantissa))
      mantissa >>>= trailingZeros
      _scale -= trailingZeros
    }
    // Calculating the new unscaled value and the new scale
    if ((bits >> 63) != 0) {
      mantissa = -mantissa
    }
    val mantissaBits = bitLength(mantissa)
    if (_scale < 0) {
      _bitLength = if (mantissaBits == 0) 0 else mantissaBits - _scale
      if (_bitLength < 64) {
        _smallValue = mantissa << (-_scale)
      } else {
        intVal = new BigInteger(1,mantissa ).shiftLeft(-scale)
      }
      _scale = 0
    } else if (_scale > 0) {
      // m * 2^e =  (m * 5^(-e)) * 10^e
      if (_scale < LONG_FIVE_POW.length &&
        mantissaBits + LONG_FIVE_POW_BIT_LENGTH(_scale) < 64) {
        _smallValue = mantissa * LONG_FIVE_POW(_scale)
        _bitLength = bitLength(_smallValue)
      } else {
        setUnscaledValue(Multiplication.multiplyByFivePow(BigInteger.valueOf(mantissa), _scale))
      }
    } else {
      _smallValue = mantissa
      _bitLength = mantissaBits
    }
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the 64bit double
   * {@code val}. The constructed big decimal is equivalent to the given
   * double. For example, {@code new BigDecimal(0.1)} is equal to {@code
   * 0.1000000000000000055511151231257827021181583404541015625}. This happens
   * as {@code 0.1} cannot be represented exactly in binary.
   * <p>
   * To generate a big decimal instance which is equivalent to {@code 0.1} use
   * the {@code BigDecimal(String)} constructor.
   *
   * @param dVal
   *            double value to be converted to a {@code BigDecimal} instance.
   * @param mc
   *            rounding mode and precision for the result of this operation.
   * @throws NumberFormatException
   *             if {@code val} is infinity or not a number.
   * @throws ArithmeticException
   *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
   *             UNNECESSARY} and the new big decimal cannot be represented
   *             within the given precision without rounding.
   */
  def this(dVal: Double, mc: MathContext) {
    this(dVal)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a given unscaled value
   * {@code unscaledVal} and a given scale. The value of this instance is
   * {@code unscaledVal * 10<sup>-scale</sup>}).
   *
   * @throws NullPointerException
     *             if {@code unscaledVal == null}.
   */
  def this(unscaledVal: BigInteger, scale: Int) {
    this()
    if (unscaledVal == null) {
      throw new NullPointerException("unscaledVal == null")
    }
    this._scale = scale
    this.setUnscaledValue(unscaledVal)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the given big integer
   * {@code val}. The scale of the result is {@code 0}.
   */
  def this(bi: BigInteger) {
    this(bi, 0)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the given big integer
   * {@code bi}. The scale of the result is {@code 0}.
   *
   * @param mc
   *            rounding mode and precision for the result of this operation.
   * @throws ArithmeticException
   *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
   *             UNNECESSARY} and the new big decimal cannot be represented
   *             within the given precision without rounding.
   */
  def this(bi: BigInteger, mc: MathContext) {
    this(bi)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from a given unscaled value
   * {@code unscaledVal} and a given scale. The value of this instance is
   * {@code unscaledVal * 10<sup>-scale</sup>). The result is rounded according
     * to the specified math context.
     *
     * @param mc
     *            rounding mode and precision for the result of this operation.
     * @throws ArithmeticException
     *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
     *             UNNECESSARY} and the new big decimal cannot be represented
   *             within the given precision without rounding.
   * @throws NullPointerException
     *             if {@code unscaledVal == null}.
   */
  def this(unscaledVal: BigInteger, scale: Int, mc: MathContext) {
    this(unscaledVal, scale)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the given int
   * {@code val}. The scale of the result is 0.
   *
   * @param iVal
     *            int value to be converted to a {@code BigDecimal} instance.
   */
  def this(iVal: Int) {
    this(iVal, 0)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the given int {@code
   * val}. The scale of the result is {@code 0}. The result is rounded
   * according to the specified math context.
   *
   * @param iVal
     *            int value to be converted to a {@code BigDecimal} instance.
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @throws ArithmeticException
     *             if {@code mc.precision > 0} and {@code c.roundingMode ==
     *             UNNECESSARY} and the new big decimal cannot be represented
                               *             within the given precision without rounding.
   */
  def this(iVal: Int, mc: MathContext) {
    this(iVal, 0)
    this.inplaceRound(mc)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the given long {@code
   * val}. The scale of the result is {@code 0}.
   *
   * @param lVal
     *            long value to be converted to a {@code BigDecimal} instance.
   */
  def this(lVal: Long) {
    this(lVal, 0)
  }

  /**
   * Constructs a new {@code BigDecimal} instance from the given long {@code
   * val}. The scale of the result is {@code 0}. The result is rounded
   * according to the specified math context.
   *
   * @param lVal
     *            long value to be converted to a {@code BigDecimal} instance.
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @throws ArithmeticException
     *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
     *             UNNECESSARY} and the new big decimal cannot be represented
                               *             within the given precision without rounding.
   */
  def this(lVal: Long, mc: MathContext) {
    this(lVal)
    this.inplaceRound(mc)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this + augend}.
   * The scale of the result is the maximum of the scales of the two
   * arguments.
   *
   * @param augend
     *            value to be added to {@code this}.
   * @return {@code this + augend}.
   * @throws NullPointerException
     *             if {@code augend == null}.
   */
  def add(augend: BigDecimal): BigDecimal = {
    val diffScale = this._scale - augend._scale
    // Fast return when some operand is zero
    if (this.isZero) {
      if (diffScale <= 0) {
        return augend
      }
      if (augend.isZero) {
        return this
      }
    } else if (augend.isZero) {
      if (diffScale >= 0) {
        return this
      }
    }
    if (diffScale == 0) {
      if (Math.max(this._bitLength, augend._bitLength) + 1 < 64) {
        return valueOf(this._smallValue + augend._smallValue, this._scale)
      }
      new BigDecimal(this.getUnscaledValue.add(augend.getUnscaledValue), this._scale)
    } else if (diffScale > 0) {
      addAndMult10(this, augend, diffScale)
    } else {
      addAndMult10(augend, this, -diffScale)
    }
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this + augend}.
   * The result is rounded according to the passed context {@code mc}.
   *
   * @param augend
     *            value to be added to {@code this}.
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code this + augend}.
   * @throws NullPointerException
     *             if {@code augend == null} or {@code mc == null}.
   */
  def add(augend: BigDecimal, mc: MathContext): BigDecimal = {
    var larger: BigDecimal = null // operand with the largest unscaled value
    var smaller: BigDecimal = null // operand with the smallest unscaled value
    var tempBI: BigInteger = null
    val diffScale = this._scale.toLong - augend._scale
    var largerSignum: Int = 0
    if (augend.isZero || this.isZero || (mc.precision == 0)) {
      return add(augend).round(mc)
    }
    // Cases where there is room for optimizations
    if (this.approxPrecision() < diffScale - 1) {
      larger = augend
      smaller = this
    } else if (augend.approxPrecision() < -diffScale - 1) {
      larger = this
      smaller = augend
    } else {// No optimization is done
      return add(augend).round(mc)
    }
    if (mc.precision >= larger.approxPrecision()) {
      // No optimization is done
      return add(augend).round(mc)
    }
    // Cases where it's unnecessary to add two numbers with very different scales
    largerSignum = larger.signum()
    if (largerSignum == smaller.signum()) {
      tempBI = Multiplication.multiplyByPositiveInt(larger.getUnscaledValue, 10)
        .add(BigInteger.valueOf(largerSignum))
    } else {
      tempBI = larger.getUnscaledValue.subtract(BigInteger.valueOf(largerSignum))
      tempBI = Multiplication.multiplyByPositiveInt(tempBI, 10).add(BigInteger.valueOf(largerSignum * 9))
    }
    // Rounding the improved adding
    larger = new BigDecimal(tempBI, larger._scale + 1)
    larger.round(mc)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this - subtrahend}.
   * The scale of the result is the maximum of the scales of the two arguments.
   *
   * @param subtrahend
     *            value to be subtracted from {@code this}.
   * @return {@code this - subtrahend}.
   * @throws NullPointerException
     *             if {@code subtrahend == null}.
   */
  def subtract(subtrahend: BigDecimal): BigDecimal = {
    var diffScale = this._scale - subtrahend._scale
    // Fast return when some operand is zero
    if (this.isZero) {
      if (diffScale <= 0) {
        return subtrahend.negate()
      }
      if (subtrahend.isZero) {
        return this
      }
    } else if (subtrahend.isZero) {
      if (diffScale >= 0) {
        return this
      }
    }
    if (diffScale == 0) {
      if (Math.max(this._bitLength, subtrahend._bitLength) + 1 < 64) {
        return valueOf(this._smallValue - subtrahend._smallValue, this._scale)
      }
      new BigDecimal(this.getUnscaledValue.subtract(subtrahend.getUnscaledValue), this._scale)
    } else if (diffScale > 0) {
      if (diffScale < LONG_POWERS_OF_TEN.length &&
        Math.max(this._bitLength, subtrahend._bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(diffScale)) +
          1 <
          64) {
        return valueOf(this._smallValue -
          subtrahend._smallValue * LONG_POWERS_OF_TEN(diffScale), this._scale)
      }
      new BigDecimal(this.getUnscaledValue.subtract(Multiplication.multiplyByTenPow(subtrahend.getUnscaledValue,
        diffScale)), this._scale)
    } else {
      diffScale = -diffScale
      if (diffScale < LONG_POWERS_OF_TEN.length &&
        Math.max(this._bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(diffScale), subtrahend._bitLength) +
          1 <
          64) {
        return valueOf(this._smallValue * LONG_POWERS_OF_TEN(diffScale) -
          subtrahend._smallValue, subtrahend._scale)
      }
      new BigDecimal(Multiplication.multiplyByTenPow(this.getUnscaledValue, diffScale)
        .subtract(subtrahend.getUnscaledValue), subtrahend._scale)
    }
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this - subtrahend}.
   * The result is rounded according to the passed context {@code mc}.
   *
   * @param subtrahend
     *            value to be subtracted from {@code this}.
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code this - subtrahend}.
   * @throws NullPointerException
     *             if {@code subtrahend == null} or {@code mc == null}.
   */
  def subtract(subtrahend: BigDecimal, mc: MathContext): BigDecimal = {
    val diffScale = subtrahend._scale - this._scale.toLong
    var thisSignum: Int = 0
    var leftOperand: BigDecimal = null // it will be only the left operand (this)
    var tempBI: BigInteger = null
    // Some operand is zero or the precision is infinity
    if (subtrahend.isZero || this.isZero || (mc.precision == 0)) {
      return subtract(subtrahend).round(mc)
    }
    // Now:   this != 0   and   subtrahend != 0
    if (subtrahend.approxPrecision() < diffScale - 1) {
      // Cases where it is unnecessary to subtract two numbers with very different scales
      if (mc.precision < this.approxPrecision()) {
        thisSignum = this.signum()
        if (thisSignum != subtrahend.signum()) {
          tempBI = Multiplication.multiplyByPositiveInt(this.getUnscaledValue, 10)
            .add(BigInteger.valueOf(thisSignum))
        } else {
          tempBI = this.getUnscaledValue.subtract(BigInteger.valueOf(thisSignum))
          tempBI = Multiplication.multiplyByPositiveInt(tempBI, 10).add(BigInteger.valueOf(thisSignum * 9))
        }
        // Rounding the improved subtracting
        leftOperand = new BigDecimal(tempBI, this._scale + 1)
        return leftOperand.round(mc)
      }
    }
    // No optimization is done
    subtract(subtrahend).round(mc)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this *
     * multiplicand}. The scale of the result is the sum of the scales of the
   * two arguments.
   *
   * @param multiplicand
     *            value to be multiplied with {@code this}.
   * @return {@code this * multiplicand}.
   * @throws NullPointerException
     *             if {@code multiplicand == null}.
   */
  def multiply(multiplicand: BigDecimal): BigDecimal = {
    val newScale = this._scale.toLong + multiplicand._scale
    if (this.isZero || multiplicand.isZero) {
      return zeroScaledBy(newScale)
    }
    if (this._bitLength + multiplicand._bitLength < 64) {
      return valueOf(this._smallValue * multiplicand._smallValue, safeLongToInt(newScale))
    }
    new BigDecimal(this.getUnscaledValue.multiply(multiplicand.getUnscaledValue), safeLongToInt(newScale))
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this *
     * multiplicand}. The result is rounded according to the passed context
   * {@code mc}.
   *
   * @param multiplicand
     *            value to be multiplied with {@code this}.
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code this * multiplicand}.
   * @throws NullPointerException
     *             if {@code multiplicand == null} or {@code mc == null}.
   */
  def multiply(multiplicand: BigDecimal, mc: MathContext): BigDecimal = {
    val result = multiply(multiplicand)
    result.inplaceRound(mc)
    result
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this / divisor}.
   * As scale of the result the parameter {@code scale} is used. If rounding
   * is required to meet the specified scale, then the specified rounding mode
   * {@code roundingMode} is applied.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param scale
     *            the scale of the result returned.
   * @param roundingMode
     *            rounding mode to be used to round the result.
   * @return {@code this / divisor} rounded according to the given rounding
   *         mode.
   * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws IllegalArgumentException
     *             if {@code roundingMode} is not a valid rounding mode.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code roundingMode == ROUND_UNNECESSARY} and rounding is
   *             necessary according to the given scale.
   */
  def divide(divisor: BigDecimal, scale: Int, roundingMode: Int): BigDecimal = {
    divide(divisor, scale, RoundingMode.valueOf(roundingMode))
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this / divisor}.
   * As scale of the result the parameter {@code scale} is used. If rounding
   * is required to meet the specified scale, then the specified rounding mode
   * {@code roundingMode} is applied.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param scale
     *            the scale of the result returned.
   * @param roundingMode
     *            rounding mode to be used to round the result.
   * @return {@code this / divisor} rounded according to the given rounding
   *         mode.
   * @throws NullPointerException
     *             if {@code divisor == null} or {@code roundingMode == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code roundingMode == RoundingMode.UNNECESSAR}Y and
   *             rounding is necessary according to the given scale and given
   *             precision.
   */
  def divide(divisor: BigDecimal, scale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (roundingMode == null) {
      throw new NullPointerException("roundingMode == null")
    }
    if (divisor.isZero) {
      throw new ArithmeticException("Division by zero")
    }
    val diffScale:Long = (this._scale.toLong - divisor._scale) - scale

    // Check whether the diffScale will fit into an int. See http://b/17393664.
    if (bitLength(diffScale) > 32) {
      throw new ArithmeticException("Unable to perform divisor / dividend scaling: the difference in scale is too" +
        " big (" +
        diffScale +
        ")")
    }
    if (this._bitLength < 64 && divisor._bitLength < 64) {
      if (diffScale == 0) {
        return dividePrimitiveLongs(this._smallValue, divisor._smallValue, scale, roundingMode)
      } else if (diffScale > 0) {
        if (diffScale < LONG_POWERS_OF_TEN.length &&
          divisor._bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(diffScale.toInt) <
            64) {
          return dividePrimitiveLongs(this._smallValue, divisor._smallValue * LONG_POWERS_OF_TEN(diffScale.toInt),
            scale, roundingMode)
        }
      } else { // diffScale < 0
        if (-diffScale < LONG_POWERS_OF_TEN.length &&
          this._bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(-diffScale.toInt) <
            64) {
          return dividePrimitiveLongs(this._smallValue * LONG_POWERS_OF_TEN(-diffScale.toInt),
            divisor._smallValue, scale, roundingMode)
        }
      }
    }

    var scaledDividend = this.getUnscaledValue
    var scaledDivisor = divisor.getUnscaledValue
    if (diffScale > 0) {
      scaledDivisor = Multiplication.multiplyByTenPow(scaledDivisor, diffScale.toInt)
    } else if (diffScale < 0) {
      scaledDividend = Multiplication.multiplyByTenPow(scaledDividend, -diffScale.toInt)
    }
    divideBigIntegers(scaledDividend, scaledDivisor, scale, roundingMode)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this / divisor}.
   * The scale of the result is the scale of {@code this}. If rounding is
   * required to meet the specified scale, then the specified rounding mode
   * {@code roundingMode} is applied.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param roundingMode
     *            rounding mode to be used to round the result.
   * @return {@code this / divisor} rounded according to the given rounding
   *         mode.
   * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws IllegalArgumentException
     *             if {@code roundingMode} is not a valid rounding mode.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code roundingMode == ROUND_UNNECESSARY} and rounding is
   *             necessary according to the scale of this.
   */
  def divide(divisor: BigDecimal, roundingMode: Int): BigDecimal = {
    divide(divisor, _scale, RoundingMode.valueOf(roundingMode))
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this / divisor}.
   * The scale of the result is the scale of {@code this}. If rounding is
   * required to meet the specified scale, then the specified rounding mode
   * {@code roundingMode} is applied.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param roundingMode
     *            rounding mode to be used to round the result.
   * @return {@code this / divisor} rounded according to the given rounding
   *         mode.
   * @throws NullPointerException
     *             if {@code divisor == null} or {@code roundingMode == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code roundingMode == RoundingMode.UNNECESSARY} and
   *             rounding is necessary according to the scale of this.
   */
  def divide(divisor: BigDecimal, roundingMode: RoundingMode): BigDecimal = divide(divisor, _scale, roundingMode)

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this / divisor}.
   * The scale of the result is the difference of the scales of {@code this}
   * and {@code divisor}. If the exact result requires more digits, then the
   * scale is adjusted accordingly. For example, {@code 1/128 = 0.0078125}
   * which has a scale of {@code 7} and precision {@code 5}.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @return {@code this / divisor}.
   * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if the result cannot be represented exactly.
   */
  def divide(divisor: BigDecimal): BigDecimal = {
    var p = this.getUnscaledValue
    var q = divisor.getUnscaledValue
    var gcd: BigInteger = null // greatest common divisor between 'p' and 'q'
    var quotAndRem: Array[BigInteger] = null // quotient and remainder
    val diffScale:Long = _scale.toLong - divisor._scale
    var newScale: Int = 0
    var k: Int = 0 // number of factors "2" in 'q'
    var l = 0      // number of factors "5" in 'q'
    var i = 1
    val lastPow = FIVE_POW.length - 1
    if (divisor.isZero) {
      throw new ArithmeticException("Division by zero")
    }
    if (p.signum() == 0) {
      return zeroScaledBy(diffScale)
    }
    // To divide both by the GCD
    gcd = p.gcd(q)
    p = p.divide(gcd)
    q = q.divide(gcd)
    // To simplify all "2" factors of q, dividing by 2^k
    k = q.getLowestSetBit
    // To simplify all "5" factors of q, dividing by 5^l
    q = q.shiftRight(k)
    def loop:Unit = do {
      quotAndRem = q.divideAndRemainder(FIVE_POW(i))
      if (quotAndRem(1).signum() == 0) {
        l += i
        if (i < lastPow) {
          i += 1
        }
        q = quotAndRem(0)
      } else {
        if (i == 1) {
          //break changed to loop
          return
        }
        i = 1
      }
    } while (true)
    loop
    // If  abs(q) != 1  then the quotient is periodic
    if (q.abs() != BigInteger.ONE) {
      throw new ArithmeticException("Non-terminating decimal expansion; no exact representable decimal result")
    }
    // The sign of the is fixed and the quotient will be saved in 'p'
    if (q.signum() < 0) {
      p = p.negate()
    }
    // Checking if the new scale is out of range
    newScale = safeLongToInt(diffScale + Math.max(k, l))
    // k >= 0  and  l >= 0  implies that  k - l  is in the 32-bit range
    i = k - l
    p = if (i > 0) Multiplication.multiplyByFivePow(p, i) else p.shiftLeft(-i)
    new BigDecimal(p, newScale)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this / divisor}.
   * The result is rounded according to the passed context {@code mc}. If the
   * passed math context specifies precision {@code 0}, then this call is
   * equivalent to {@code this.divide(divisor)}.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code this / divisor}.
   * @throws NullPointerException
     *             if {@code divisor == null} or {@code mc == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code mc.getRoundingMode() == UNNECESSARY} and rounding
   *             is necessary according {@code mc.getPrecision()}.
   */
  def divide(divisor: BigDecimal, mc: MathContext): BigDecimal = {
    /* Calculating how many zeros must be append to 'dividend'
        * to obtain a  quotient with at least 'mc.precision()' digits */
    val trailingZeros:Long = mc.precision + 2L + divisor.approxPrecision() - approxPrecision()
    val diffScale:Long = _scale.toLong - divisor._scale
    var newScale:Long = diffScale
    var compRem: Int = 0
    var i = 1
    val lastPow = TEN_POW.length - 1
    var integerQuot: BigInteger = null
    var quotAndRem:Array[BigInteger] = Array(getUnscaledValue)
    // In special cases it reduces the problem to call the dual method
    if ((mc.precision == 0) || this.isZero || divisor.isZero) {
      return this.divide(divisor)
    }
    if (trailingZeros > 0) {
      // To append trailing zeros at end of dividend
      quotAndRem(0) = getUnscaledValue.multiply(Multiplication.powerOf10(trailingZeros))
      newScale += trailingZeros
    }
    quotAndRem = quotAndRem(0).divideAndRemainder(divisor.getUnscaledValue)
    integerQuot = quotAndRem(0)
    // Calculating the exact quotient with at least 'mc.precision()' digits
    if (quotAndRem(1).signum() != 0) {
      // Checking if:   2 * remainder >= divisor ?
      compRem = quotAndRem(1).shiftLeftOneBit().compareTo(divisor.getUnscaledValue)
      // quot := quot * 10 + r;     with 'r' in {-6,-5,-4, 0,+4,+5,+6}
      integerQuot = integerQuot.multiply(BigInteger.TEN).add(BigInteger.valueOf(quotAndRem(0).signum() * (5 + compRem)))
      newScale += 1
    } else {
      // To strip trailing zeros until the preferred scale is reached

      def loop:Unit = while (!integerQuot.testBit(0)) {
        quotAndRem = integerQuot.divideAndRemainder(TEN_POW(i))
        if ((quotAndRem(1).signum() == 0) && (newScale - i >= diffScale)) {
          newScale -= i
          if (i < lastPow) {
            i += 1
          }
          integerQuot = quotAndRem(0)
        } else {
          if (i == 1) {
            //break changed to loop
            return
          }
          i = 1
        }
      }
      loop
    }
    // To perform rounding
    new BigDecimal(integerQuot, safeLongToInt(newScale), mc)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is the integral part of
   * {@code this / divisor}. The quotient is rounded down towards zero to the
   * next integer. For example, {@code 0.5/0.2 = 2}.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @return integral part of {@code this / divisor}.
   * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   */
  def divideToIntegralValue(divisor: BigDecimal): BigDecimal = {
    var integralValue: BigInteger = null
    var powerOfTen: BigInteger = null
    var quotAndRem = Array(getUnscaledValue)
    var newScale: Long = this._scale.toLong - divisor._scale
    var tempScale: Long = 0
    var i = 1
    val lastPow = TEN_POW.length - 1
    if (divisor.isZero) {
      throw new ArithmeticException("Division by zero")
    }
    if ((divisor.approxPrecision() + newScale > this.approxPrecision() + 1L) ||
      this.isZero) {
      /* If the divisor's integer part is greater than this's integer part,
       * the result must be zero with the appropriate scale */
      integralValue = BigInteger.ZERO
    } else if (newScale == 0) {
      integralValue = getUnscaledValue.divide(divisor.getUnscaledValue)
    } else if (newScale > 0) {
      powerOfTen = Multiplication.powerOf10(newScale)
      integralValue = getUnscaledValue.divide(divisor.getUnscaledValue.multiply(powerOfTen))
      integralValue = integralValue.multiply(powerOfTen)
    } else {// (newScale < 0)
      powerOfTen = Multiplication.powerOf10(-newScale)
      integralValue = getUnscaledValue.multiply(powerOfTen).divide(divisor.getUnscaledValue)
      // To strip trailing zeros approximating to the preferred scale
      def loop:Unit = while (!integralValue.testBit(0)) {
        quotAndRem = integralValue.divideAndRemainder(TEN_POW(i))
        if ((quotAndRem(1).signum() == 0) && (tempScale - i >= newScale)) {
          tempScale -= i
          if (i < lastPow) {
            i += 1
          }
          integralValue = quotAndRem(0)
        } else {
          if (i == 1) {
            //break changed to loop
            return
          }
          i = 1
        }
      }
      loop
      newScale = tempScale
    }
    if ((integralValue.signum() == 0)) zeroScaledBy(newScale) else new BigDecimal(integralValue, safeLongToInt(newScale))
  }

  /**
   * Returns a new {@code BigDecimal} whose value is the integral part of
   * {@code this / divisor}. The quotient is rounded down towards zero to the
   * next integer. The rounding mode passed with the parameter {@code mc} is
   * not considered. But if the precision of {@code mc > 0} and the integral
   * part requires more digits, then an {@code ArithmeticException} is thrown.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param mc
     *            math context which determines the maximal precision of the
   *            result.
   * @return integral part of {@code this / divisor}.
   * @throws NullPointerException
     *             if {@code divisor == null} or {@code mc == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code mc.getPrecision() > 0} and the result requires more
   *             digits to be represented.
   */
  def divideToIntegralValue(divisor: BigDecimal, mc: MathContext): BigDecimal = {
    val mcPrecision = mc.precision
    val diffPrecision = this.precision() - divisor.precision()
    val lastPow = TEN_POW.length - 1
    val diffScale = this._scale.toLong - divisor._scale
    var newScale = diffScale
    val quotPrecision = diffPrecision - diffScale + 1
    var quotAndRem = new Array[BigInteger](2)
    // In special cases it call the dual method
    if ((mcPrecision == 0) || this.isZero || divisor.isZero) {
      return this.divideToIntegralValue(divisor)
    }
    if (quotPrecision <= 0) {
      quotAndRem(0) = BigInteger.ZERO
    } else if (diffScale == 0) {
      quotAndRem(0) = this.getUnscaledValue.divide(divisor.getUnscaledValue)
    } else if (diffScale > 0) {
      quotAndRem(0) = this.getUnscaledValue.divide(divisor.getUnscaledValue.multiply(Multiplication.powerOf10(diffScale)))
      // To chose  10^newScale  to get a quotient with at least 'mc.precision()' digits
      newScale = Math.min(diffScale, Math.max(mcPrecision - quotPrecision + 1, 0))
      quotAndRem(0) = quotAndRem(0).multiply(Multiplication.powerOf10(newScale))
    } else {
      /* To calculate the minimum power of ten, such that the quotient
       *   (u1 * 10^exp) / u2   has at least 'mc.precision()' digits. */
      var exp = Math.min(-diffScale, Math.max(mcPrecision.toLong - diffPrecision, 0))
      var compRemDiv: Long = 0l
      quotAndRem = this.getUnscaledValue.multiply(Multiplication.powerOf10(exp))
        .divideAndRemainder(divisor.getUnscaledValue)
      newScale += exp// To fix the scale
      exp = -newScale// The remaining power of ten
      // If after division there is a remainder...
      if ((quotAndRem(1).signum() != 0) && (exp > 0)) {
        compRemDiv = new BigDecimal(quotAndRem(1)).precision() + exp - divisor.precision()
        if (compRemDiv == 0) {
          quotAndRem(1) = quotAndRem(1).multiply(Multiplication.powerOf10(exp))
            .divide(divisor.getUnscaledValue)
          compRemDiv = Math.abs(quotAndRem(1).signum())
        }
        if (compRemDiv > 0) {
          throw new ArithmeticException("Division impossible")
        }
      }
    }
    // Fast return if the quotient is zero
    if (quotAndRem(0).signum() == 0) {
      return zeroScaledBy(diffScale)
    }
    var strippedBI = quotAndRem(0)
    val integralValue = new BigDecimal(quotAndRem(0))
    var resultPrecision = integralValue.precision()
    // To strip trailing zeros until the specified precision is reached
    var i = 1
    def loop: Unit = while (!strippedBI.testBit(0)) {
      quotAndRem = strippedBI.divideAndRemainder(TEN_POW(i))
      if ((quotAndRem(1).signum() == 0) &&
        ((resultPrecision - i >= mcPrecision) || (newScale - i >= diffScale))) {
        resultPrecision -= i
        newScale -= i
        if (i < lastPow) {
          i += 1
        }
        strippedBI = quotAndRem(0)
      } else {
        if (i == 1) {
          //break changed to loop
          return
        }
        i = 1
      }
    }
    loop
    // To check if the result fit in 'mc.precision()' digits
    if (resultPrecision > mcPrecision) {
      throw new ArithmeticException("Division impossible")
    }
    integralValue._scale = safeLongToInt(newScale)
    integralValue.setUnscaledValue(strippedBI)
    integralValue
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this % divisor}.
   * <p>
   * The remainder is defined as {@code this -
     * this.divideToIntegralValue(divisor) * divisor}.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @return {@code this % divisor}.
   * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   */
  def remainder(divisor: BigDecimal): BigDecimal = divideAndRemainder(divisor)(1)

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this % divisor}.
   * <p>
   * The remainder is defined as {@code this -
     * this.divideToIntegralValue(divisor) * divisor}.
   * <p>
   * The specified rounding mode {@code mc} is used for the division only.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param mc
     *            rounding mode and precision to be used.
   * @return {@code this % divisor}.
   * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @throws ArithmeticException
     *             if {@code mc.getPrecision() > 0} and the result of {@code
     *             this.divideToIntegralValue(divisor, mc)} requires more digits
   *             to be represented.
   */
  def remainder(divisor: BigDecimal, mc: MathContext): BigDecimal = divideAndRemainder(divisor, mc)(1)

  /**
   * Returns a {@code BigDecimal} array which contains the integral part of
   * {@code this / divisor} at index 0 and the remainder {@code this %
     * divisor} at index 1. The quotient is rounded down towards zero to the
   * next integer.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @return {@code [this.divideToIntegralValue(divisor),
     *         this.remainder(divisor)]}.
                                        * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @see #divideToIntegralValue
   * @see #remainder
   */
  def divideAndRemainder(divisor: BigDecimal): Array[BigDecimal] = {
    val quotAndRem = new Array[BigDecimal](2)
    quotAndRem(0) = this.divideToIntegralValue(divisor)
    quotAndRem(1) = this.subtract(quotAndRem(0).multiply(divisor))
    quotAndRem
  }

  /**
   * Returns a {@code BigDecimal} array which contains the integral part of
   * {@code this / divisor} at index 0 and the remainder {@code this %
     * divisor} at index 1. The quotient is rounded down towards zero to the
   * next integer. The rounding mode passed with the parameter {@code mc} is
   * not considered. But if the precision of {@code mc > 0} and the integral
   * part requires more digits, then an {@code ArithmeticException} is thrown.
   *
   * @param divisor
     *            value by which {@code this} is divided.
   * @param mc
     *            math context which determines the maximal precision of the
   *            result.
   * @return {@code [this.divideToIntegralValue(divisor),
     *         this.remainder(divisor)]}.
                                        * @throws NullPointerException
     *             if {@code divisor == null}.
   * @throws ArithmeticException
     *             if {@code divisor == 0}.
   * @see #divideToIntegralValue
   * @see #remainder
   */
  def divideAndRemainder(divisor: BigDecimal, mc: MathContext): Array[BigDecimal] = {
    val quotAndRem = new Array[BigDecimal](2)
    quotAndRem(0) = this.divideToIntegralValue(divisor, mc)
    quotAndRem(1) = this.subtract(quotAndRem(0).multiply(divisor))
    quotAndRem
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this<sup>n</sup>}. The
   * scale of the result is {@code n * this.scale()}.
   *
   * <p>{@code x.pow(0)} returns {@code 1}, even if {@code x == 0}.
   *
   * <p>Implementation Note: The implementation is based on the ANSI standard
   * X3.274-1996 algorithm.
   *
   * @throws ArithmeticException
     *             if {@code n < 0} or {@code n > 999999999}.
   */
  def pow(n: Int): BigDecimal = {
    if (n == 0) {
      return ONE
    }
    if ((n < 0) || (n > 999999999)) {
      throw new ArithmeticException("Invalid operation")
    }
    val newScale = _scale * n.toLong
    if (isZero) zeroScaledBy(newScale) else new BigDecimal(getUnscaledValue.pow(n), safeLongToInt(newScale))
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this<sup>n</sup>}. The
   * result is rounded according to the passed context {@code mc}.
   *
   * <p>Implementation Note: The implementation is based on the ANSI standard
   * X3.274-1996 algorithm.
   *
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @throws ArithmeticException
     *             if {@code n < 0} or {@code n > 999999999}.
   */
  def pow(n: Int, mc: MathContext): BigDecimal = {
    val m = Math.abs(n)
    val mcPrecision = mc.precision
    val elength = Math.log10(m).toInt + 1
    var oneBitMask: Int = 0
    var accum: BigDecimal = null
    var newPrecision = mc

    // In particular cases, it reduces the problem to call the other 'pow()'
    if ((n == 0) || (isZero && (n > 0))) {
      return pow(n)
    }
    if ((m > 999999999) || ((mcPrecision == 0) && (n < 0)) || ((mcPrecision > 0) && (elength > mcPrecision))) {
      throw new ArithmeticException("Invalid operation")
    }
    if (mcPrecision > 0) {
      newPrecision = new MathContext(mcPrecision + elength + 1, mc.roundingMode)
    }

    // The result is calculated as if 'n' were positive
    accum = round(newPrecision)
    oneBitMask = java.lang.Integer.highestOneBit(m) >> 1

    while (oneBitMask > 0) {
      accum = accum.multiply(accum, newPrecision)
      if ((m & oneBitMask) == oneBitMask) {
        accum = accum.multiply(this, newPrecision)
      }
      oneBitMask >>= 1
    }
    // If 'n' is negative, the value is divided into 'ONE'
    if (n < 0) {
      accum = ONE.divide(accum, newPrecision)
    }
    // The final value is rounded to the destination precision
    accum.inplaceRound(mc)
    accum
  }

  /**
   * Returns a {@code BigDecimal} whose value is the absolute value of
   * {@code this}. The scale of the result is the same as the scale of this.
   */
  def abs(): BigDecimal = {
    if (signum() < 0) negate() else this
  }

  /**
   * Returns a {@code BigDecimal} whose value is the absolute value of
   * {@code this}. The result is rounded according to the passed context
   * {@code mc}.
   */
  def abs(mc: MathContext): BigDecimal = {
    val result = if (signum() < 0) negate() else new BigDecimal(getUnscaledValue, _scale)
    result.inplaceRound(mc)
    result
  }

  /**
   * Returns a new {@code BigDecimal} whose value is the {@code -this}. The
   * scale of the result is the same as the scale of this.
   *
   * @return {@code -this}
   */
  def negate(): BigDecimal = {
    if (_bitLength < 63 ||
      (_bitLength == 63 && _smallValue != java.lang.Long.MIN_VALUE)) {
      valueOf(-_smallValue, _scale)
    }else
    new BigDecimal(getUnscaledValue.negate(), _scale)
  }

  /**
   * Returns a new {@code BigDecimal} whose value is the {@code -this}. The
   * result is rounded according to the passed context {@code mc}.
   *
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code -this}
   */
  def negate(mc: MathContext): BigDecimal = {
    val result = negate()
    result.inplaceRound(mc)
    result
  }

  /**
   * Returns a new {@code BigDecimal} whose value is {@code +this}. The scale
   * of the result is the same as the scale of this.
   *
   * @return {@code this}
   */
  def plus(): BigDecimal = this

  /**
   * Returns a new {@code BigDecimal} whose value is {@code +this}. The result
   * is rounded according to the passed context {@code mc}.
   *
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code this}, rounded
   */
  def plus(mc: MathContext): BigDecimal = round(mc)

  /**
   * Returns the sign of this {@code BigDecimal}.
   *
   * @return {@code -1} if {@code this < 0},
   *         {@code 0} if {@code this == 0},
   *         {@code 1} if {@code this > 0}.
   */
  def signum(): Int = {
    if (_bitLength < 64) {
      if (_smallValue < 0) -1
      else if (_smallValue > 0) 1
      else 0
    } else
      getUnscaledValue.signum()
  }

  private def isZero(): Boolean = _bitLength == 0 && this._smallValue != -1

  /**
   * Returns the precision of this {@code BigDecimal}. The precision is the
   * number of decimal digits used to represent this decimal. It is equivalent
   * to the number of digits of the unscaled value. The precision of {@code 0}
   * is {@code 1} (independent of the scale).
   *
   * @return the precision of this {@code BigDecimal}.
   */
  def precision(): Int = {
    // Return the cached value if we have one.
    if (_precision != 0) {
      return _precision
    }
    if (_bitLength == 0) {
      _precision = 1
    } else if (_bitLength < 64) {
      _precision = decimalDigitsInLong(_smallValue)
    } else {
      var decimalDigits = 1 + ((_bitLength - 1) * LOG10_2).toInt
      // If after division the number isn't zero, there exists an additional digit
      if (getUnscaledValue.divide(Multiplication.powerOf10(decimalDigits))
        .signum() !=
        0) {
        decimalDigits += 1
      }
      _precision = decimalDigits
    }
    _precision
  }

  private def decimalDigitsInLong(value: Long): Int = {
    if (value == java.lang.Long.MIN_VALUE) {
      19 // special case required because abs(MIN_VALUE) == MIN_VALUE
    } else {
      val index = Arrays.binarySearch(LONG_POWERS_OF_TEN, Math.abs(value))
      if (index < 0) -index - 1 else index + 1
    }
  }

  /**
   * Returns the unscaled value (mantissa) of this {@code BigDecimal} instance
   * as a {@code BigInteger}. The unscaled value can be computed as
   * {@code this * 10<sup>scale</sup>}.
   */
  def unscaledValue(): BigInteger = getUnscaledValue

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this}, rounded
   * according to the passed context {@code mc}.
   * <p>
   * If {@code mc.precision = 0}, then no rounding is performed.
   * <p>
   * If {@code mc.precision > 0} and {@code mc.roundingMode == UNNECESSARY},
   * then an {@code ArithmeticException} is thrown if the result cannot be
   * represented exactly within the given precision.
   *
   * @param mc
     *            rounding mode and precision for the result of this operation.
   * @return {@code this} rounded according to the passed context.
   * @throws ArithmeticException
     *             if {@code mc.precision > 0} and {@code mc.roundingMode ==
     *             UNNECESSARY} and this cannot be represented within the given
                               *             precision.
   */
  def round(mc: MathContext): BigDecimal = {
    val thisBD = new BigDecimal(getUnscaledValue, _scale)
    thisBD.inplaceRound(mc)
    thisBD
  }

  /**
   * Returns a new {@code BigDecimal} instance with the specified scale.
   * <p>
   * If the new scale is greater than the old scale, then additional zeros are
   * added to the unscaled value. In this case no rounding is necessary.
   * <p>
   * If the new scale is smaller than the old scale, then trailing digits are
   * removed. If these trailing digits are not zero, then the remaining
   * unscaled value has to be rounded. For this rounding operation the
   * specified rounding mode is used.
   *
   * @param newScale
     *            scale of the result returned.
   * @param roundingMode
     *            rounding mode to be used to round the result.
   * @return a new {@code BigDecimal} instance with the specified scale.
   * @throws NullPointerException
     *             if {@code roundingMode == null}.
   * @throws ArithmeticException
     *             if {@code roundingMode == ROUND_UNNECESSARY} and rounding is
   *             necessary according to the given scale.
   */
  def setScale(newScale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (roundingMode == null) {
      throw new NullPointerException("roundingMode == null")
    }
    val diffScale = newScale - _scale.toLong
    if (diffScale == 0) {
      return this
    }
    if (diffScale > 0) {
      if (diffScale < LONG_POWERS_OF_TEN.length &&
        (this._bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(diffScale.toInt)) <
          64) {
        return valueOf(this._smallValue * LONG_POWERS_OF_TEN(diffScale.toInt), newScale)
      }
      return new BigDecimal(Multiplication.multiplyByTenPow(getUnscaledValue, diffScale.toInt), newScale)
    }
    if (this._bitLength < 64 && -diffScale < LONG_POWERS_OF_TEN.length) {
      return dividePrimitiveLongs(this._smallValue, LONG_POWERS_OF_TEN(-diffScale.toInt), newScale,
        roundingMode)
    }
    divideBigIntegers(this.getUnscaledValue, Multiplication.powerOf10(-diffScale), newScale, roundingMode)
  }

  /**
   * Returns the scale of this {@code BigDecimal}. The scale is the number of
   * digits behind the decimal point. The value of this {@code BigDecimal} is
   * the {@code unsignedValue * 10<sup>-scale</sup>}. If the scale is negative,
   * then this {@code BigDecimal} represents a big integer.
   *
   * @return the scale of this {@code BigDecimal}.
   */
  def scale():Int = _scale

  /**
   * Returns a new {@code BigDecimal} instance with the specified scale.
   * <p>
   * If the new scale is greater than the old scale, then additional zeros are
   * added to the unscaled value. In this case no rounding is necessary.
   * <p>
   * If the new scale is smaller than the old scale, then trailing digits are
   * removed. If these trailing digits are not zero, then the remaining
   * unscaled value has to be rounded. For this rounding operation the
   * specified rounding mode is used.
   *
   * @param newScale
     *            scale of the result returned.
   * @param roundingMode
     *            rounding mode to be used to round the result.
   * @return a new {@code BigDecimal} instance with the specified scale.
   * @throws IllegalArgumentException
     *             if {@code roundingMode} is not a valid rounding mode.
   * @throws ArithmeticException
     *             if {@code roundingMode == ROUND_UNNECESSARY} and rounding is
   *             necessary according to the given scale.
   */
  def setScale(newScale: Int, roundingMode: Int): BigDecimal = {
    setScale(newScale, RoundingMode.valueOf(roundingMode))
  }

  /**
   * Returns a new {@code BigDecimal} instance with the specified scale. If
   * the new scale is greater than the old scale, then additional zeros are
   * added to the unscaled value. If the new scale is smaller than the old
   * scale, then trailing zeros are removed. If the trailing digits are not
   * zeros then an ArithmeticException is thrown.
   * <p>
   * If no exception is thrown, then the following equation holds: {@code
   * x.setScale(s).compareTo(x) == 0}.
   *
   * @param newScale
     *            scale of the result returned.
   * @return a new {@code BigDecimal} instance with the specified scale.
   * @throws ArithmeticException
     *             if rounding would be necessary.
   */
  def setScale(newScale: Int): BigDecimal = {
    setScale(newScale, RoundingMode.UNNECESSARY)
  }

  /**
   * Returns a new {@code BigDecimal} instance where the decimal point has
   * been moved {@code n} places to the left. If {@code n < 0} then the
   * decimal point is moved {@code -n} places to the right.
   *
   * <p>The result is obtained by changing its scale. If the scale of the result
   * becomes negative, then its precision is increased such that the scale is
   * zero.
   *
   * <p>Note, that {@code movePointLeft(0)} returns a result which is
   * mathematically equivalent, but which has {@code scale >= 0}.
   */
  def movePointLeft(n: Int): BigDecimal = movePoint(_scale + n.toLong)

  private def movePoint(newScale: Long): BigDecimal = {
    if (isZero) {
      return zeroScaledBy(Math.max(newScale, 0))
    }
    /*
         * When: 'n'== Integer.MIN_VALUE isn't possible to call to
         * movePointRight(-n) since -Integer.MIN_VALUE == Integer.MIN_VALUE
         */
    if (newScale >= 0) {
      if (_bitLength < 64) {
        return valueOf(_smallValue, safeLongToInt(newScale))
      }
      return new BigDecimal(getUnscaledValue, safeLongToInt(newScale))
    }
    if (-newScale < LONG_POWERS_OF_TEN.length &&
      _bitLength + LONG_POWERS_OF_TEN_BIT_LENGTH(-newScale.toInt) <
        64) {
      return valueOf(_smallValue * LONG_POWERS_OF_TEN(-newScale.toInt), 0)
    }
    new BigDecimal(Multiplication.multiplyByTenPow(getUnscaledValue, safeLongToInt(-newScale)), 0)
  }

  /**
   * Returns a new {@code BigDecimal} instance where the decimal point has
   * been moved {@code n} places to the right. If {@code n < 0} then the
   * decimal point is moved {@code -n} places to the left.
   *
   * <p>The result is obtained by changing its scale. If the scale of the result
   * becomes negative, then its precision is increased such that the scale is
   * zero.
   *
   * <p>Note, that {@code movePointRight(0)} returns a result which is
   * mathematically equivalent, but which has scale >= 0.
   */
  def movePointRight(n: Int): BigDecimal = movePoint(_scale - n.toLong)

  /**
   * Returns a new {@code BigDecimal} whose value is {@code this * 10<sup>n</sup>}.
   * The scale of the result is {@code this.scale()} - {@code n}.
   * The precision of the result is the precision of {@code this}.
   *
   * <p>This method has the same effect as {@link #movePointRight}, except that
   * the precision is not changed.
   */
  def scaleByPowerOfTen(n: Int): BigDecimal = {
    val newScale = _scale - n.toLong
    if (_bitLength < 64) {
      //Taking care when a 0 is to be scaled
      if (_smallValue == 0) {
        return zeroScaledBy(newScale)
      }
      return valueOf(_smallValue, safeLongToInt(newScale))
    }
    new BigDecimal(getUnscaledValue, safeLongToInt(newScale))
  }

  /**
   * Returns a new {@code BigDecimal} instance with the same value as {@code
   * this} but with a unscaled value where the trailing zeros have been
   * removed. If the unscaled value of {@code this} has n trailing zeros, then
   * the scale and the precision of the result has been reduced by n.
   *
   * @return a new {@code BigDecimal} instance equivalent to this where the
   *         trailing zeros of the unscaled value have been removed.
   */
  def stripTrailingZeros(): BigDecimal = {
    var i = 1
    val lastPow = TEN_POW.length - 1
    var newScale = _scale
    if (isZero) {
      // Preserve RI compatibility, so BigDecimal.equals (which checks
      // value *and* scale) continues to work.
      return this
    }
    var strippedBI = getUnscaledValue
    var quotAndRem: Array[BigInteger] = null
    // while the number is even...

    def loop: Unit = while (!strippedBI.testBit(0)) {
      // To divide by 10^i
      quotAndRem = strippedBI.divideAndRemainder(TEN_POW(i))
      // To look the remainder
      if (quotAndRem(1).signum() == 0) {
        // To adjust the scale
        newScale -= i
        if (i < lastPow) {
          // To set to the next power
          i += 1
        }
        strippedBI = quotAndRem(0)
      } else {
        if (i == 1) {
          // 'this' has no more trailing zeros
          //break changed to loop
          return
        }
        // To set to the smallest power of ten
        i = 1
      }
    }
    loop
    new BigDecimal(strippedBI, safeLongToInt(newScale))
  }

  /**
   * Compares this {@code BigDecimal} with {@code val}. Returns one of the
   * three values {@code 1}, {@code 0}, or {@code -1}. The method behaves as
   * if {@code this.subtract(val)} is computed. If this difference is > 0 then
   * 1 is returned, if the difference is < 0 then -1 is returned, and if the
   * difference is 0 then 0 is returned. This means, that if two decimal
   * instances are compared which are equal in value but differ in scale, then
   * these two instances are considered as equal.
   *
   * @param bi
     *            value to be compared with {@code this}.
   * @return {@code 1} if {@code this > val}, {@code -1} if {@code this < val},
   *         {@code 0} if {@code this == val}.
   * @throws NullPointerException
     *             if {@code val == null}.
   */
  def compareTo(bi: BigDecimal): Int = {
    val thisSign = signum()
    val valueSign = bi.signum()
    if (thisSign == valueSign) {
      if (this._scale == bi._scale && this._bitLength < 64 && bi._bitLength < 64) {
        return if (_smallValue < bi._smallValue) -1 else if (_smallValue > bi._smallValue) 1 else 0
      }
      val diffScale = this._scale.toLong - bi._scale
      val diffPrecision = this.approxPrecision() - bi.approxPrecision()
      if (diffPrecision > diffScale + 1) {
        thisSign
      } else if (diffPrecision < diffScale - 1) {
        -thisSign
      } else {// thisSign == val.signum()  and  diffPrecision is aprox. diffScale
        var thisUnscaled = this.getUnscaledValue
        var valUnscaled = bi.getUnscaledValue
        // If any of both precision is bigger, append zeros to the shorter one
        if (diffScale < 0) {
          thisUnscaled = thisUnscaled.multiply(Multiplication.powerOf10(-diffScale))
        } else if (diffScale > 0) {
          valUnscaled = valUnscaled.multiply(Multiplication.powerOf10(diffScale))
        }
        thisUnscaled.compareTo(valUnscaled)
      }
    } else if (thisSign < valueSign) {
      -1
    } else {
      1
    }
  }
  /**
   * Returns {@code true} if {@code x} is a {@code BigDecimal} instance and if
   * this instance is equal to this big decimal. Two big decimals are equal if
   * their unscaled value and their scale is equal. For example, 1.0
   * (10*10<sup>-1</sup>) is not equal to 1.00 (100*10<sup>-2</sup>). Similarly, zero
   * instances are not equal if their scale differs.
   */
  override def equals(x: Any): Boolean = x match{
    case that: BigDecimal ⇒ that._scale == this._scale &&
      (if (_bitLength < 64) that._smallValue == this._smallValue else this.intVal == that.intVal)
    case _ ⇒ false
  }

  /**
   * Returns the minimum of this {@code BigDecimal} and {@code val}.
   *
   * @param bd  value to be used to compute the minimum with this.
   * @return {@code min(this, val}.
   * @throws NullPointerException  if {@code val == null}.
   */
  def min(bd: BigDecimal): BigDecimal = {
    if ((compareTo(bd) <= 0)) this else bd
  }

  /**
   * Returns the maximum of this {@code BigDecimal} and {@code val}.
   *
   * @param bd  value to be used to compute the maximum with this.
   * @return {@code max(this, val}.
   * @throws NullPointerException  if {@code val == null}.
   */
  def max(bd: BigDecimal): BigDecimal = {
    if ((compareTo(bd) >= 0)) this else bd
  }

  /**
   * Returns a hash code for this {@code BigDecimal}.
   *
   * @return hash code for {@code this}.
   */
  override def hashCode(): Int = {
    if (_hashCode != 0) {
      return _hashCode
    }
    if (_bitLength < 64) {
      _hashCode = (_smallValue & 0xffffffff).toInt
      _hashCode = 33 * _hashCode + ((_smallValue >> 32) & 0xffffffff).toInt
      _hashCode = 17 * _hashCode + _scale
      return _hashCode
    }
    _hashCode = 17 * intVal.hashCode + _scale
    _hashCode
  }

  /**
   * Returns a canonical string representation of this {@code BigDecimal}. If
   * necessary, scientific notation is used. This representation always prints
   * all significant digits of this value.
   * <p>
   * If the scale is negative or if {@code scale - precision >= 6} then
   * scientific notation is used.
   *
   * @return a string representation of {@code this} in scientific notation if
   *         necessary.
   */
  override def toString(): String =  {
    if (toStringImage != null) {
      return toStringImage
    }
    if (_bitLength < 32) {
      toStringImage = Conversion.toDecimalScaledString(_smallValue, _scale)
      return toStringImage
    }
    val intString:String = getUnscaledValue.toString
    if (_scale == 0) {
      return intString
    }
    val begin = if (getUnscaledValue.signum() < 0) 2 else 1
    var end = intString.length
    val exponent:Long = -_scale.toLong + end - begin
    val result = new java.lang.StringBuilder()
    result.append(intString)
    if ((_scale > 0) && (exponent >= -6)) {
      if (exponent >= 0) {
        result.insert(end - _scale, '.')
      } else {
        result.insert(begin - 1, "0.")
        result.insert(begin + 1, CH_ZEROS, 0, -exponent.toInt - 1)
      }
    } else {
      if (end - begin >= 1) {
        result.insert(begin, '.')
        end += 1
      }
      result.insert(end, 'E')
      if (exponent > 0) {
        end += 1
        result.insert(end, '+')
      }
      end += 1
      result.insert(end, java.lang.Long.toString(exponent))
    }
    toStringImage = result.toString
    toStringImage
  }

  /**
   * Returns a string representation of this {@code BigDecimal}. This
   * representation always prints all significant digits of this value.
   * <p>
   * If the scale is negative or if {@code scale - precision >= 6} then
   * engineering notation is used. Engineering notation is similar to the
   * scientific notation except that the exponent is made to be a multiple of
   * 3 such that the integer part is >= 1 and < 1000.
   *
   * @return a string representation of {@code this} in engineering notation
   *         if necessary.
   */
  def toEngineeringString(): String = {
    val intString = getUnscaledValue.toString
    if (_scale == 0) {
      return intString
    }
    var begin = if (getUnscaledValue.signum() < 0) 2 else 1
    var end:Int = intString.length
    var exponent:Long = -_scale.toLong + end - begin
    val result = new java.lang.StringBuilder(intString)
    if ((_scale > 0) && (exponent >= -6)) {
      if (exponent >= 0) {
        result.insert(end - _scale, '.')
      } else {
        result.insert(begin - 1, "0.")
        result.insert(begin + 1, CH_ZEROS, 0, -exponent.toInt - 1)
      }
    } else {
      val delta = end - begin
      var rem = (exponent % 3).toInt
      if (rem != 0) {
        if (getUnscaledValue.signum() == 0) {
          rem = if (rem < 0) -rem else 3 - rem
          exponent += rem
        } else {
          rem = if (rem < 0) rem + 3 else rem
          exponent -= rem
          begin += rem
        }
        if (delta < 3) {
          var i = rem - delta
          while (i > 0) {
            result.insert(end, '0')
            end += 1
            i -= 1
          }
        }
      }
      if (end - begin >= 1) {
        result.insert(begin, '.')
        end += 1
      }
      if (exponent != 0) {
        result.insert(end, 'E')
        if (exponent > 0) {
          end += 1
          result.insert(end, '+')
        }
        end += 1
        result.insert(end, java.lang.Long.toString(exponent))
      }
    }
    result.toString
  }

  /**
   * Returns a string representation of this {@code BigDecimal}. No scientific
   * notation is used. This methods adds zeros where necessary.
   * <p>
   * If this string representation is used to create a new instance, this
   * instance is generally not identical to {@code this} as the precision
   * changes.
   * <p>
   * {@code x.equals(new BigDecimal(x.toPlainString())} usually returns
   * {@code false}.
   * <p>
   * {@code x.compareTo(new BigDecimal(x.toPlainString())} returns {@code 0}.
   *
   * @return a string representation of {@code this} without exponent part.
   */
  def toPlainString(): String = {
    val intStr = getUnscaledValue.toString
    if ((_scale == 0) || (isZero && (_scale < 0))) {
      return intStr
    }
    val begin = if (signum() < 0) 1 else 0
    var delta = _scale
    // We take space for all digits, plus a possible decimal point, plus 'scale'
    val result = new java.lang.StringBuilder(intStr.length + 1 + Math.abs(_scale))
    if (begin == 1) {
      // If the number is negative, we insert a '-' character at front
      result.append('-')
    }
    if (_scale > 0) {
      delta -= (intStr.length - begin)
      if (delta >= 0) {
        result.append("0.")
        // To append zeros after the decimal point
        while (delta > CH_ZEROS.length) {
          result.append(CH_ZEROS)
          delta -= CH_ZEROS.length
        }
        result.append(CH_ZEROS, 0, delta)
        result.append(intStr.substring(begin))
      } else {
        delta = begin - delta
        result.append(intStr.substring(begin, delta))
        result.append('.')
        result.append(intStr.substring(delta))
      }
    } else {// (scale <= 0)
      result.append(intStr.substring(begin))
      // To append trailing zeros
      while (delta < -CH_ZEROS.length) {
        result.append(CH_ZEROS)
        delta += CH_ZEROS.length
      }
      result.append(CH_ZEROS, 0, -delta)
    }
    result.toString
  }

  /**
   * Returns this {@code BigDecimal} as a big integer instance. A fractional
   * part is discarded.
   *
   * @return this {@code BigDecimal} as a big integer instance.
   */
  def toBigInteger(): BigInteger = {
    if ((_scale == 0) || isZero) {
      getUnscaledValue
    } else if (_scale < 0) {
      getUnscaledValue.multiply(Multiplication.powerOf10(-_scale.toLong))
    } else {
      getUnscaledValue.divide(Multiplication.powerOf10(_scale))
    }
  }

  /**
   * Returns this {@code BigDecimal} as a big integer instance if it has no
   * fractional part. If this {@code BigDecimal} has a fractional part, i.e.
   * if rounding would be necessary, an {@code ArithmeticException} is thrown.
   *
   * @return this {@code BigDecimal} as a big integer value.
   * @throws ArithmeticException
     *             if rounding is necessary.
   */
  def toBigIntegerExact(): BigInteger = {
    if ((_scale == 0) || isZero) {
      getUnscaledValue
    } else if (_scale < 0) {
      getUnscaledValue.multiply(Multiplication.powerOf10(-_scale.toLong))
    } else {// (scale > 0)
      var integerAndFraction: Array[BigInteger] = null
      // An optimization before do a heavy division
      if ((_scale > approxPrecision()) || (_scale > getUnscaledValue.getLowestSetBit)) {
        throw new ArithmeticException("Rounding necessary")
      }
      integerAndFraction = getUnscaledValue.divideAndRemainder(Multiplication.powerOf10(_scale))
      if (integerAndFraction(1).signum() != 0) {
        // It exists a non-zero fractional part
        throw new ArithmeticException("Rounding necessary")
      }
      integerAndFraction(0)
    }
  }

  /**
   * Returns this {@code BigDecimal} as an long value. Any fractional part is
   * discarded. If the integral part of {@code this} is too big to be
   * represented as an long, then {@code this % 2<sup>64</sup>} is returned.
   */
  override def longValue(): Long = {
    /*
     * If scale <= -64 there are at least 64 trailing bits zero in
     * 10^(-scale). If the scale is positive and very large the long value
     * could be zero.
     */
    if ((_scale <= -64) || (_scale > approxPrecision())) 0L else toBigInteger().longValue()
  }

  /**
   * Returns this {@code BigDecimal} as a long value if it has no fractional
   * part and if its value fits to the int range ([-2<sup>63</sup>..2<sup>63</sup>-1]). If
   * these conditions are not met, an {@code ArithmeticException} is thrown.
   *
   * @throws ArithmeticException
     *             if rounding is necessary or the number doesn't fit in a long.
   */
  def longValueExact(): Long = valueExact(64)

  /**
   * Returns this {@code BigDecimal} as an int value. Any fractional part is
   * discarded. If the integral part of {@code this} is too big to be
   * represented as an int, then {@code this % 2<sup>32</sup>} is returned.
   */
  override def intValue(): Int = {
    /*
     * If scale <= -32 there are at least 32 trailing bits zero in
     * 10^(-scale). If the scale is positive and very large the long value
     * could be zero.
     */
    if ((_scale <= -32) || (_scale > approxPrecision())) 0 else toBigInteger().intValue()
  }

  /**
   * Returns this {@code BigDecimal} as a int value if it has no fractional
   * part and if its value fits to the int range ([-2<sup>31</sup>..2<sup>31</sup>-1]). If
   * these conditions are not met, an {@code ArithmeticException} is thrown.
   *
   * @throws ArithmeticException
     *             if rounding is necessary or the number doesn't fit in an int.
   */
  def intValueExact(): Int = valueExact(32).toInt

  /**
   * Returns this {@code BigDecimal} as a short value if it has no fractional
   * part and if its value fits to the short range ([-2<sup>15</sup>..2<sup>15</sup>-1]). If
   * these conditions are not met, an {@code ArithmeticException} is thrown.
   *
   * @throws ArithmeticException
     *             if rounding is necessary of the number doesn't fit in a short.
   */
  def shortValueExact(): Short = valueExact(16).toShort

  /**
   * Returns this {@code BigDecimal} as a byte value if it has no fractional
   * part and if its value fits to the byte range ([-128..127]). If these
   * conditions are not met, an {@code ArithmeticException} is thrown.
   *
   * @throws ArithmeticException
     *             if rounding is necessary or the number doesn't fit in a byte.
   */
  def byteValueExact(): Byte = valueExact(8).toByte

  /**
   * Returns this {@code BigDecimal} as a float value. If {@code this} is too
   * big to be represented as an float, then {@code Float.POSITIVE_INFINITY}
   * or {@code Float.NEGATIVE_INFINITY} is returned.
   * <p>
   * Note, that if the unscaled value has more than 24 significant digits,
   * then this decimal cannot be represented exactly in a float variable. In
   * this case the result is rounded.
   * <p>
   * For example, if the instance {@code x1 = new BigDecimal("0.1")} cannot be
   * represented exactly as a float, and thus {@code x1.equals(new
     * BigDecimal(x1.floatValue())} returns {@code false} for this case.
   * <p>
   * Similarly, if the instance {@code new BigDecimal(16777217)} is converted
   * to a float, the result is {@code 1.6777216E}7.
   *
   * @return this {@code BigDecimal} as a float value.
   */
  override def floatValue(): Float = {
    /* A similar code like in doubleValue() could be repeated here,
     * but this simple implementation is quite efficient. */
    var floatResult:Float = signum()
    val powerOfTwo = this._bitLength - (_scale / LOG10_2).toLong
    if ((powerOfTwo < -149) || (floatResult == 0.0f)) {
      // Cases which 'this' is very small
      floatResult *= 0.0f
    } else if (powerOfTwo > 129) {
      // Cases which 'this' is very large
      floatResult *= java.lang.Float.POSITIVE_INFINITY
    } else {
      floatResult = doubleValue().toFloat
    }
    floatResult
  }

  /**
   * Returns this {@code BigDecimal} as a double value. If {@code this} is too
   * big to be represented as an float, then {@code Double.POSITIVE_INFINITY}
   * or {@code Double.NEGATIVE_INFINITY} is returned.
   * <p>
   * Note, that if the unscaled value has more than 53 significant digits,
   * then this decimal cannot be represented exactly in a double variable. In
   * this case the result is rounded.
   * <p>
   * For example, if the instance {@code x1 = new BigDecimal("0.1")} cannot be
   * represented exactly as a double, and thus {@code x1.equals(new
     * BigDecimal(x1.doubleValue())} returns {@code false} for this case.
   * <p>
   * Similarly, if the instance {@code new BigDecimal(9007199254740993L)} is
   * converted to a double, the result is {@code 9.007199254740992E15}.
   * <p>
   *
   * @return this {@code BigDecimal} as a double value.
   */
  override def doubleValue(): Double = {
    //return java.lang.Double.parseDouble(this.toString ());
    val sign:Int = signum()
    var exponent:Int = 1076  // bias + 53
    var lowestSetBit: Int = 0
    var discardedSize: Int = 0
    val powerOfTwo = this._bitLength - (_scale / LOG10_2).toLong
    var bits: Long = 0l // IEEE-754 Standard
    var tempBits: Long = 0l // for temporal calculations
    var mantissa: BigInteger = null

    if ((powerOfTwo < -1074) || (sign == 0)) {
      // Cases which 'this' is very small
      return sign * 0.0d
    } else if (powerOfTwo > 1025) {
      // Cases which 'this' is very large
      return sign * java.lang.Double.POSITIVE_INFINITY
    }
    mantissa = getUnscaledValue.abs()
    if (_scale <= 0) {
      mantissa = mantissa.multiply(Multiplication.powerOf10(-_scale))
    } else {
      var quotAndRem: Array[BigInteger] = null
      val powerOfTen:BigInteger = Multiplication.powerOf10(_scale)
      val k = 100 - powerOfTwo.toInt
      var compRem: Int = 0
      if (k > 0) {
        /* Computing (mantissa * 2^k) , where 'k' is a enough big
         * power of '2' to can divide by 10^s */
        mantissa = mantissa.shiftLeft(k)
        exponent -= k
      }
      // Computing (mantissa * 2^k) / 10^s
      quotAndRem = mantissa.divideAndRemainder(powerOfTen)
      // To check if the fractional part >= 0.5
      compRem = quotAndRem(1).shiftLeftOneBit().compareTo(powerOfTen)
      // To add two rounded bits at end of mantissa
      mantissa = quotAndRem(0).shiftLeft(2).add(BigInteger.valueOf((compRem * (compRem + 3)) / 2 + 1))
      exponent -= 2
    }
    lowestSetBit = mantissa.getLowestSetBit
    discardedSize = mantissa.bitLength() - 54
    if (discardedSize > 0) {// (n > 54)
      bits = mantissa.shiftRight(discardedSize).longValue()
      tempBits = bits
      if ((((bits & 1) == 1) && (lowestSetBit < discardedSize)) ||
        ((bits & 3) == 3)) {
        bits += 2
      }
    } else {// (n <= 54)
      bits = mantissa.longValue() << -discardedSize
      tempBits = bits
      if ((bits & 3) == 3) {
        bits += 2
      }
    }
    // Testing bit 54 to check if the carry creates a new binary digit
    if ((bits & 0x40000000000000L) == 0) {
      // To drop the last bit of mantissa (first discarded)
      bits >>= 1
      exponent += discardedSize
    } else {// #bits = 54
      bits >>= 2
      exponent += (discardedSize + 1)
    }
    // To test if the 53-bits number fits in 'double'
    if (exponent > 2046) {// (exponent - bias > 1023)
      return sign * java.lang.Double.POSITIVE_INFINITY
    } else if (exponent <= 0) {// (exponent - bias <= -1023)
      // Denormalized numbers (having exponent == 0)
       if (exponent < -53) {
        return sign * 0.0d
      }
      // -1076 <= exponent - bias <= -1023
      // To discard '- exponent + 1' bits
      bits = tempBits >> 1
      tempBits = bits & (-1L >>> (63 + exponent))
      bits >>= (-exponent)
      // To test if after discard bits, a new carry is generated
      if (((bits & 3) == 3) ||
        (((bits & 1) == 1) && (tempBits != 0) && (lowestSetBit < discardedSize))) {
        bits += 1
      }
      exponent = 0
      bits >>= 1
    }

    // Construct the 64 double bits: [sign(1), exponent(11), mantissa(52)]
    bits = (sign & 0x8000000000000000L) | (exponent.toLong << 52) |
      (bits & 0xFFFFFFFFFFFFFL)
    java.lang.Double.longBitsToDouble(bits)
  }

  /**
   * Returns the unit in the last place (ULP) of this {@code BigDecimal}
   * instance. An ULP is the distance to the nearest big decimal with the same
   * precision.
   *
   * <p>The amount of a rounding error in the evaluation of a floating-point
   * operation is often expressed in ULPs. An error of 1 ULP is often seen as
   * a tolerable error.
   *
   * <p>For class {@code BigDecimal}, the ULP of a number is simply 10<sup>-scale</sup>.
   * For example, {@code new BigDecimal(0.1).ulp()} returns {@code 1E-55}.
   *
   * @return unit in the last place (ULP) of this {@code BigDecimal} instance.
   */
  def ulp(): BigDecimal = valueOf(1, _scale)

  /* Private Methods */
  /**
   * It does all rounding work of the public method
   * {@code round(MathContext)}, performing an inplace rounding
   * without creating a new object.
   *
   * @param mc
     *            the {@code MathContext} for perform the rounding.
   * @see #round(MathContext)
   */
  private def inplaceRound(mc: MathContext): Unit = {
    val mcPrecision = mc.precision
    if (approxPrecision() < mcPrecision || mcPrecision == 0) {
      return
    }
    val discardedPrecision = precision() - mcPrecision
    // If no rounding is necessary it returns immediately
    if (discardedPrecision <= 0) {
      return
    }
    // When the number is small perform an efficient rounding
    if (this._bitLength < 64) {
      smallRound(mc, discardedPrecision)
      return
    }
    // Getting the integer part and the discarded fraction
    val sizeOfFraction:BigInteger = Multiplication.powerOf10(discardedPrecision)
    val integerAndFraction:Array[BigInteger] = getUnscaledValue.divideAndRemainder(sizeOfFraction)
    var newScale = _scale.toLong - discardedPrecision
    var compRem: Int = 0
    // If the discarded fraction is non-zero, perform rounding
    if (integerAndFraction(1).signum() != 0) {
      // To check if the discarded fraction >= 0.5
      compRem = integerAndFraction(1).abs().shiftLeftOneBit().compareTo(sizeOfFraction)
      // To look if there is a carry
      compRem = roundingBehavior(if (integerAndFraction(0).testBit(0)) 1 else 0, integerAndFraction(1).signum() * (5 + compRem),
        mc.roundingMode)
      if (compRem != 0) {
        integerAndFraction(0) = integerAndFraction(0).add(BigInteger.valueOf(compRem))
      }
      val tempBD: BigDecimal  = new BigDecimal(integerAndFraction(0))
      // If after to add the increment the precision changed, we normalize the size
      if (tempBD.precision() > mcPrecision) {
        integerAndFraction(0) = integerAndFraction(0).divide(BigInteger.TEN)
        newScale -= 1
      }
    }
    // To update all internal fields
    _scale = safeLongToInt(newScale)
    _precision = mcPrecision
    setUnscaledValue(integerAndFraction(0))
  }

  /**
   * This method implements an efficient rounding for numbers which unscaled
   * value fits in the type {@code long}.
   *
   * @param mc
     *            the context to use
   * @param discardedPrecision
     *            the number of decimal digits that are discarded
   * @see #round(MathContext)
   */
  private def smallRound(mc: MathContext, discardedPrecision: Int): Unit = {
    val sizeOfFraction: Long = LONG_POWERS_OF_TEN(discardedPrecision)
    var newScale: Long       = _scale.toLong - discardedPrecision
    val unscaledVal: Long    = _smallValue
    // Getting the integer part and the discarded fraction
    var integer: Long        = unscaledVal / sizeOfFraction
    val fraction: Long       = unscaledVal % sizeOfFraction
    var compRem: Int = 0
    // If the discarded fraction is non-zero perform rounding

    if (fraction != 0) {
      // To check if the discarded fraction >= 0.5
      compRem = longCompareTo(Math.abs(fraction) * 2, sizeOfFraction)
      // To look if there is a carry
      integer += roundingBehavior(integer.toInt & 1, Util.signum(fraction) * (5 + compRem),
        mc.roundingMode)
      // If after to add the increment the precision changed, we normalize the size
      if (Math.log10(Math.abs(integer)) >= mc.precision) {
        integer /= 10
        newScale -= 1
      }
    }
    // To update all internal fields
    _scale = safeLongToInt(newScale)
    _precision = mc.precision
    _smallValue = integer
    _bitLength = bitLength(integer)
    intVal = null
  }

  /**
   * If {@code intVal} has a fractional part throws an exception,
   * otherwise it counts the number of bits of value and checks if it's out of
   * the range of the primitive type. If the number fits in the primitive type
   * returns this number as {@code long}, otherwise throws an
   * exception.
   *
   * @param bitLengthOfType
     *            number of bits of the type whose value will be calculated
   *            exactly
   * @return the exact value of the integer part of {@code BigDecimal}
   *         when is possible
   * @throws ArithmeticException when rounding is necessary or the
   *             number don't fit in the primitive type
   */
  private def valueExact(bitLengthOfType: Int): Long = {
    val bigInteger = toBigIntegerExact()
    if (bigInteger.bitLength() < bitLengthOfType) {
      return bigInteger.longValue()
    }
    throw new ArithmeticException("Rounding necessary")
  }

  /**
   * If the precision already was calculated it returns that value, otherwise
   * it calculates a very good approximation efficiently . Note that this
   * value will be {@code precision()} or {@code precision()-1}
   * in the worst case.
   *
   * @return an approximation of {@code precision()} value
   */
  private def approxPrecision(): Int = {
    if (_precision > 0) _precision else ((this._bitLength - 1) * LOG10_2).toInt + 1
  }

  /**
   * Assigns all transient fields upon deserialization of a
   * {@code BigDecimal} instance (bitLength and smallValue). The transient
   * field precision is assigned lazily.
   */
  private def readObject(in: ObjectInputStream): Unit = {
    in.defaultReadObject()
    this._bitLength = intVal.bitLength()
    if (this._bitLength < 64) {
      this._smallValue = intVal.longValue()
    }
  }

  /**
   * Prepares this {@code BigDecimal} for serialization, i.e. the
   * non-transient field {@code intVal} is assigned.
   */
  private def writeObject(out: ObjectOutputStream): Unit = {
    getUnscaledValue
    out.defaultWriteObject()
  }

  private def getUnscaledValue(): BigInteger = {
    if (intVal == null) {
      intVal = BigInteger.valueOf(_smallValue)
    }
    intVal
  }

  private def setUnscaledValue(unscaledVal: BigInteger):Unit = {
    this.intVal = unscaledVal
    this._bitLength = unscaledVal.bitLength()
    if (this._bitLength < 64) {
      this._smallValue = unscaledVal.longValue()
    }
  }
}



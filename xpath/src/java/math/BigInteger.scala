/*
 *  Ported by Alistair Johnson from  https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/math/BigInteger.java
 */
package java.math


import java.io.Serializable
import java.util.Random

/**
 * Companion object for {@code BigInteger}
 */
object BigInteger {

  /**
   * The {@code BigInteger} constant 1.
   */
  val ONE = new BigInteger(1, 1)

  /**
   * The {@code BigInteger} constant 10.
   */
  val TEN = new BigInteger(1, 10)

  /**
   * The {@code BigInteger} constant 0.
   */
  val ZERO = new BigInteger(0, 0)

  /**
   * The {@code BigInteger} constant 0 used for comparison.
   */
  val EQUALS = 0

  /**
   * The {@code BigInteger} constant 1 used for comparison.
   */
  val GREATER = 1

  /**
   * The {@code BigInteger} constant -1 used for comparison.
   */
  val LESS = -1

  /**
   * The {@code BigInteger} constant -1.
   */
  val MINUS_ONE = new BigInteger(-1, 1)

  /**
   * 2^32.
   */
  val POW32 = 4294967296d

  /**
   * All the {@code BigInteger} numbers in the range [0,10] are cached.
   */
  val SMALL_VALUES = Array(
    ZERO, ONE, new BigInteger(1, 2), new BigInteger(1, 3),
    new BigInteger(1, 4), new BigInteger(1, 5), new BigInteger(1, 6),
    new BigInteger(1, 7), new BigInteger(1, 8), new BigInteger(1, 9), TEN)

  val TWO_POWS = new Array[BigInteger](32)

  for (i ← 0 until TWO_POWS.length) {
    TWO_POWS(i) = BigInteger.valueOf(1L << i)
  }

  /**
   * Returns a random positive {@code BigInteger} instance in the range [0,
   * 2^(bitLength)-1] which is probably prime. The probability that the returned
   * {@code BigInteger} is prime is beyond (1-1/2^80).
   * <p>
   * <b>Implementation Note:</b> Currently {@code rnd} is ignored.
   *
   * @param bitLength length of the new {@code BigInteger} in bits.
   * @param rnd random generator used to generate the new {@code BigInteger}.
   * @return probably prime random {@code BigInteger} instance.
   * @throws ArithmeticException if {@code bitLength < 2}.
   */
  def probablePrime(bitLength: Int, rnd: Random): BigInteger = new BigInteger(bitLength, 100, rnd)

  def valueOf(lVal: Long): BigInteger = {
    if (lVal < 0) {
      if (lVal != -1) {
        return new BigInteger(-1, -lVal)
      }
      MINUS_ONE
    } else if (lVal <= 10) {
      SMALL_VALUES(lVal.toInt)
    } else {
      new BigInteger(1, lVal)
    }
  }

  def getPowerOfTwo(exp: Int): BigInteger = {
    if (exp < TWO_POWS.length) {
      return TWO_POWS(exp)
    }
    val intCount = exp >> 5
    val bitN = exp & 31
    val resDigits = new Array[Int](intCount + 1)
    resDigits(intCount) = 1 << bitN
    new BigInteger(1, intCount + 1, resDigits)
  }

}

/**
 * This class represents immutable integer numbers of arbitrary length. Large
 * numbers are typically used in security applications and therefore BigIntegers
 * offer dedicated functionality like the generation of large prime numbers or
 * the computation of modular inverse.
 * <p>
 * Since the class was modeled to offer all the functionality as the
 * {@link Integer} class does, it provides even methods that operate bitwise on
 * a two's complement representation of large integers. Note however that the
 * implementations favors an internal representation where magnitude and sign
 * are treated separately. Hence such operations are inefficient and should be
 * discouraged. In simple words: Do NOT implement any bit fields based on
 * BigInteger.
 */
class BigInteger  extends Number with Comparable[BigInteger] with Serializable {
  import InternalPreconditions._
  import BigInteger._

  /**
   * The magnitude of this big integer. This array is in little endian order and
   * each "digit" is a 32-bit unsigned integer. For example: {@code 13} is
   * represented as [ 13 ] {@code -13} is represented as [ 13 ] {@code 2^32 +
   * 13} is represented as [ 13, 1 ] {@code 2^64 + 13} is represented as [ 13,
   * 0, 1 ] {@code 2^31} is represented as [ Integer.MIN_VALUE ] The magnitude
   * array may be longer than strictly necessary, which results in additional
   * trailing zeros.
   *
   * <p>TODO(jat): consider changing to 24-bit integers for better performance
   * in browsers.
   */
  @transient var digits: Array[Int] = _

  /**
   * The length of this in measured in ints. Can be less than digits.length().
   */
  @transient var numberLength: Int = _

  /**
   * The sign of this.
   */
  @transient var sign: Int = _

  @transient private var firstNonzeroDigit: Int = -2

  /**
   * Cache for the hash code.
   */
  @transient private var _hashCode: Int = 0

  /**
   * Constructs a new {@code BigInteger} from the given two's complement
   * representation. The most significant byte is the entry at index 0. The most
   * significant bit of this entry determines the sign of the new {@code
   * BigInteger} instance. The given array must not be empty.
   *
   * @param val two's complement representation of the new {@code BigInteger}.
   * @throws NullPointerException if {@code val == null}.
   * @throws NumberFormatException if the length of {@code val} is zero.
   */
  def this(byteArray: Array[Byte]) = {
    this()
    if (byteArray.length == 0) {
      throw new NumberFormatException("Zero length BigInteger")
    }

    if (byteArray(0) < 0) {
      sign = -1
      this.putBytesNegativeToIntegers(byteArray)
    } else {
      sign = 1
      this.putBytesPositiveToIntegers(byteArray)
    }

    this.cutOffLeadingZeroes()
  }


  /**
   * Constructs a new {@code BigInteger} instance with the given sign and the
   * given magnitude. The sign is given as an integer (-1 for negative, 0 for
   * zero, 1 for positive). The magnitude is specified as a byte array. The most
   * significant byte is the entry at index 0.
   *
   * @param signum sign of the new {@code BigInteger} (-1 for negative, 0 for
   *          zero, 1 for positive).
   * @param magnitude magnitude of the new {@code BigInteger} with the most
   *          significant byte first.
   * @throws NullPointerException if {@code magnitude == null}.
   * @throws NumberFormatException if the sign is not one of -1, 0, 1 or if the
   *           sign is zero and the magnitude contains non-zero entries.
   */
  def this(signum: Int, magnitude: Array[Byte]) {
    this()
    checkNotNull(magnitude)
    if ((signum < -1) || (signum > 1)) {
      throw new NumberFormatException("Invalid signum value")
    }
    if (signum == 0) {
      for (element ← magnitude if element != 0) {
        throw new NumberFormatException("signum-magnitude mismatch")
      }
    }
    if (magnitude.length == 0) {
      sign = 0
      numberLength = 1
      digits = Array(0)
    } else {
      sign = signum
      this.putBytesPositiveToIntegers(magnitude)
      this.cutOffLeadingZeroes()
    }
  }

  /**
   * Constructs a random {@code BigInteger} instance in the range [0,
   * 2^(bitLength)-1] which is probably prime. The probability that the returned
   * {@code BigInteger} is prime is beyond (1-1/2^certainty).
   *
   * @param bitLength length of the new {@code BigInteger} in bits.
   * @param certainty tolerated primality uncertainty.
   * @param rnd is an optional random generator to be used.
   * @throws ArithmeticException if {@code bitLength} < 2.
   */
  def this(bitLength: Int, certainty: Int, rnd: Random) {
    this()
    if (bitLength < 2) {
      throw new ArithmeticException("bitLength < 2")
    }
    val me = Primality.consBigInteger(bitLength, certainty, rnd)
    sign = me.sign
    numberLength = me.numberLength
    digits = me.digits
  }

  /**
   * Constructs a random non-negative {@code BigInteger} instance in the range
   * [0, 2^(numBits)-1].
   *
   * @param numBits maximum length of the new {@code BigInteger} in bits.
   * @param rnd is an optional random generator to be used.
   * @throws IllegalArgumentException if {@code numBits} < 0.
   */
  def this(numBits: Int, rnd: Random) {
    this()
    checkCriticalArgument(numBits >= 0, "numBits must be non-negative")
    if (numBits == 0) {
      sign = 0
      numberLength = 1
      digits = Array(0)
    } else {
      sign = 1
      numberLength = (numBits + 31) >> 5
      digits = new Array[Int](numberLength)
      for (i ← 0 until numberLength) {
        digits(i) = rnd.nextInt()
      }
      digits(numberLength - 1) >>>= (-numBits) & 31
      this.cutOffLeadingZeroes()
    }
  }

  /**
   * Constructs a new {@code BigInteger} instance from the string
   * representation. The string representation consists of an optional minus
   * sign followed by a non-empty sequence of digits in the specified radix. For
   * the conversion the method {@code Character.digit(char, radix)} is used.
   *
   * @param val string representation of the new {@code BigInteger}.
   * @param radix the base to be used for the conversion.
   * @throws NullPointerException if {@code val == null}.
   * @throws NumberFormatException if {@code val} is not a valid representation
   *           of a {@code BigInteger} or if {@code radix < Character.MIN_RADIX}
   *           or {@code radix > Character.MAX_RADIX}.
   */
  def this(s: String, radix: Int) {
    this()
    checkNotNull(s)
    if ((radix < java.lang.Character.MIN_RADIX) || (radix > java.lang.Character.MAX_RADIX)) {
      throw new NumberFormatException("Radix out of range")
    }
    if (s.isEmpty) {
      throw new NumberFormatException("Zero length BigInteger")
    }
    this.setFromString(s, radix)
  }

  /**
   * Constructs a new {@code BigInteger} instance from the string
   * representation. The string representation consists of an optional minus
   * sign followed by a non-empty sequence of decimal digits.
   *
   * @param s string representation of the new {@code BigInteger}.
   * @throws NullPointerException if {@code val == null}.
   * @throws NumberFormatException if {@code val} is not a valid representation
   *           of a {@code BigInteger}.
   */
  def this(s: String) {
    this(s, 10)
  }

  /**
   * Constructs a number which array is of size 1.
   *
   * @param sign the sign of the number
   * @param value the only one digit of array
   */
  def this(sign: Int, value: Int) {
    this()
    this.sign = sign
    numberLength = 1
    digits = Array(value)
  }
  
  /**
   * Creates a new {@code BigInteger} with the given sign and magnitude. This
   * constructor does not create a copy, so any changes to the reference will
   * affect the new number.
   *
   * @param signum The sign of the number represented by {@code digits}
   * @param digits The magnitude of the number
   */
  def this(signum: Int, digits: Array[Int]) {
    this()
    if (digits.length == 0) {
      this.sign = 0
      this.numberLength = 1
      this.digits = Array(0)
    } else {
      this.sign = signum
      this.numberLength = digits.length
      this.digits = digits
      this.cutOffLeadingZeroes()
    }
  }

  /**
   * Constructs a number without to create new space. This construct should be
   * used only if the three fields of representation are known.
   *
   * @param sign the sign of the number
   * @param numberLength the length of the internal array
   * @param digits a reference of some array created before
   */
  def this(sign: Int, numberLength: Int, digits: Array[Int]) {
    this()
    this.sign = sign
    this.numberLength = numberLength
    this.digits = digits
  }

  /**
   * Creates a new {@code BigInteger} whose value is equal to the specified
   * {@code long}.
   *
   * @param sign the sign of the number
   * @param val the value of the new {@code BigInteger}.
   */
  def this(sign: Int, lVal: Long) {
    this()
    this.sign = sign
    if ((lVal & 0xFFFFFFFF00000000L) == 0) {
      numberLength = 1
      digits = Array(lVal.toInt)
    } else {
      numberLength = 2
      digits = Array(lVal.toInt, (lVal >> 32).toInt)
    }
  }

  /**
   * Returns a (new) {@code BigInteger} whose value is the absolute value of
   * {@code this}.
   *
   * @return {@code abs(this)}.
   */
  def abs(): BigInteger = {
    if ((sign < 0)) new BigInteger(1, numberLength, digits) else this
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this + val}.
   *
   * @param bi value to be added to {@code this}.
   * @return {@code this + val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def add(bi: BigInteger): BigInteger = Elementary.add(this, bi)

  /**
   * Returns a new {@code BigInteger} whose value is {@code this & val}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param bi value to be and'ed with {@code this}.
   * @return {@code this & val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def and(bi: BigInteger): BigInteger = Logical.and(this, bi)

  /**
   * Returns a new {@code BigInteger} whose value is {@code this & ~val}.
   * Evaluating {@code x.andNot(val)} returns the same result as {@code
   * x.and(val.not())}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param bi value to be not'ed and then and'ed with {@code this}.
   * @return {@code this & ~val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def andNot(bi: BigInteger): BigInteger = Logical.andNot(this, bi)

  /**
   * Use {@code bitLength(0)} if you want to know the length of the binary value
   * in bits.
   * <p>
   * Returns the number of bits in the binary representation of {@code this}
   * which differ from the sign bit. If {@code this} is positive the result is
   * equivalent to the number of bits set in the binary representation of
   * {@code this}. If {@code this} is negative the result is equivalent to the
   * number of bits set in the binary representation of {@code -this-1}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @return number of bits in the binary representation of {@code this} which
   *         differ from the sign bit
   */
  def bitCount(): Int = BitLevel.bitCount(this)


  /**
   * Returns the length of the value's two's complement representation without
   * leading zeros for positive numbers / without leading ones for negative
   * values.
   * <p>
   * The two's complement representation of {@code this} will be at least
   * {@code bitLength() + 1} bits long.
   * <p>
   * The value will fit into an {@code int} if {@code bitLength() < 32} or into
   * a {@code long} if {@code bitLength() < 64}.
   *
   * @return the length of the minimal two's complement representation for
   *         {@code this} without the sign bit.
   */
  def bitLength(): Int = BitLevel.bitLength(this)

  /**
   * Returns a new {@code BigInteger} which has the same binary representation
   * as {@code this} but with the bit at position n cleared. The result is
   * equivalent to {@code this & ~(2^n)}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param n position where the bit in {@code this} has to be cleared.
   * @return {@code this & ~(2^n)}.
   * @throws ArithmeticException if {@code n < 0}.
   */
  def clearBit(n: Int): BigInteger = {
    if (testBit(n)) {
      return BitLevel.flipBit(this, n)
    }
    this
  }

  /**
   * Compares this {@code BigInteger} with {@code val}. Returns one of the three
   * values 1, 0, or -1.
   *
   * @param val value to be compared with {@code this}.
   * @return {@code 1} if {@code this > val}, {@code -1} if {@code this < val} ,
   *         {@code 0} if {@code this == val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def compareTo(bi: BigInteger): Int = {
    if (sign > bi.sign) {
      return GREATER
    }
    if (sign < bi.sign) {
      return LESS
    }
    if (numberLength > bi.numberLength) {
      return sign
    }
    if (numberLength < bi.numberLength) {
      return -bi.sign
    }
    // Equal sign and equal numberLength
    sign * Elementary.compareArrays(digits, bi.digits, numberLength)
  }


  /**
   * Returns a new {@code BigInteger} whose value is {@code this / divisor}.
   *
   * @param divisor value by which {@code this} is divided.
   * @return {@code this / divisor}.
   * @throws NullPointerException if {@code divisor == null}.
   * @throws ArithmeticException if {@code divisor == 0}.
   */
  def divide(divisor: BigInteger): BigInteger = {
    if (divisor.sign == 0) {
      throw new ArithmeticException("BigInteger divide by zero")
    }
    val divisorSign = divisor.sign
    if (divisor.isOne) {
      return if ((divisor.sign > 0)) this else this.negate()
    }
    val thisSign = sign
    val thisLen = numberLength
    val divisorLen = divisor.numberLength
    if (thisLen + divisorLen == 2) {
      var bi = (digits(0) & 0xFFFFFFFFL) / (divisor.digits(0) & 0xFFFFFFFFL)
      if (thisSign != divisorSign) {
        bi = -bi
      }
      return valueOf(bi)
    }
    val cmp = if ((thisLen != divisorLen)) (if ((thisLen > divisorLen)) 1 else -1)
    else Elementary.compareArrays(digits,
      divisor.digits, thisLen)
    if (cmp == EQUALS) {
      return if ((thisSign == divisorSign)) ONE else MINUS_ONE
    }
    if (cmp == LESS) {
      return ZERO
    }
    val resLength = thisLen - divisorLen + 1
    val resDigits = new Array[Int](resLength)
    val resSign = if ((thisSign == divisorSign)) 1 else -1
    if (divisorLen == 1) {
      Division.divideArrayByInt(resDigits, digits, thisLen, divisor.digits(0))
    } else {
      Division.divide(resDigits, resLength, digits, thisLen, divisor.digits, divisorLen)
    }
    val result = new BigInteger(resSign, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /**
   * Returns a {@code BigInteger} array which contains {@code this / divisor} at
   * index 0 and {@code this % divisor} at index 1.
   *
   * @param divisor value by which {@code this} is divided.
   * @return {@code [this / divisor, this % divisor]}.
   * @throws NullPointerException if {@code divisor == null}.
   * @throws ArithmeticException if {@code divisor == 0}.
   * @see #divide
   * @see #remainder
   */
  def divideAndRemainder(divisor: BigInteger): Array[BigInteger] = {
    val divisorSign = divisor.sign
    if (divisorSign == 0) {
      throw new ArithmeticException("BigInteger divide by zero")
    }
    val divisorLen = divisor.numberLength
    val divisorDigits = divisor.digits
    if (divisorLen == 1) {
      return Division.divideAndRemainderByInteger(this, divisorDigits(0), divisorSign)
    }

    // res[0] is a quotient and res[1] is a remainder:
    val thisDigits = digits
    val thisLen = numberLength
    val cmp = if (thisLen != divisorLen) if ((thisLen > divisorLen)) 1 else -1 else Elementary.compareArrays(thisDigits,
      divisorDigits, thisLen)
    if (cmp < 0) {
      return Array(ZERO, this)
    }
    val thisSign = sign
    val quotientLength = thisLen - divisorLen + 1
    val remainderLength = divisorLen
    val quotientSign = if ((thisSign == divisorSign)) 1 else -1
    val quotientDigits = new Array[Int](quotientLength)
    val remainderDigits = Division.divide(quotientDigits, quotientLength, thisDigits, thisLen, divisorDigits,
      divisorLen)
    val result0 = new BigInteger(quotientSign, quotientLength, quotientDigits)
    val result1 = new BigInteger(thisSign, remainderLength, remainderDigits)
    result0.cutOffLeadingZeroes()
    result1.cutOffLeadingZeroes()
    Array(result0, result1)
  }

  /**
   * Returns this {@code BigInteger} as an double value. If {@code this} is too
   * big to be represented as an double, then {@code Double.POSITIVE_INFINITY}
   * or {@code Double.NEGATIVE_INFINITY} is returned. Note, that not all
   * integers x in the range [-Double.MAX_VALUE, Double.MAX_VALUE] can be
   * represented as a double. The double representation has a mantissa of length
   * 53. For example, 2^53+1 = 9007199254740993 is returned as double
   * 9007199254740992.0.
   *
   * @return this {@code BigInteger} as a double value
   */
  override def doubleValue(): Double = {
    java.lang.Double.parseDouble(this.toString)
  }

  /**
   * Returns {@code true} if {@code x} is a BigInteger instance and if this
   * instance is equal to this {@code BigInteger}.
   *
   * @param x object to be compared with {@code this}.
   * @return true if {@code x} is a BigInteger and {@code this == x}, {@code
   *         false} otherwise.
   */
  override def equals(x: Any): Boolean = x match{
    case that: BigInteger ⇒ this.sign == that.sign && this.numberLength == that.numberLength && this.equalsArrays(that.digits)
    case _ ⇒ false
  }


  /**
   * Returns a new {@code BigInteger} which has the same binary representation
   * as {@code this} but with the bit at position n flipped. The result is
   * equivalent to {@code this ^ 2^n}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param n position where the bit in {@code this} has to be flipped.
   * @return {@code this ^ 2^n}.
   * @throws ArithmeticException if {@code n < 0}.
   */
  def flipBit(n: Int): BigInteger = {
    if (n < 0) {
      throw new ArithmeticException("Negative bit address")
    }
    BitLevel.flipBit(this, n)
  }


  /**
   * Returns this {@code BigInteger} as an float value. If {@code this} is too
   * big to be represented as an float, then {@code Float.POSITIVE_INFINITY} or
   * {@code Float.NEGATIVE_INFINITY} is returned. Note, that not all integers x
   * in the range [-Float.MAX_VALUE, Float.MAX_VALUE] can be represented as a
   * float. The float representation has a mantissa of length 24. For example,
   * 2^24+1 = 16777217 is returned as float 16777216.0.
   *
   * @return this {@code BigInteger} as a float value.
   */
  override def floatValue(): Float = {
    java.lang.Float.parseFloat(this.toString)
  }

  /**
   * Returns a new {@code BigInteger} whose value is greatest common divisor of
   * {@code this} and {@code val}. If {@code this==0} and {@code val==0} then
   * zero is returned, otherwise the result is positive.
   *
   * @param bi value with which the greatest common divisor is computed.
   * @return {@code gcd(this, val)}.
   * @throws NullPointerException if {@code val == null}.
   */
  def gcd(bi: BigInteger): BigInteger = {
    val val1 = this.abs()
    val val2 = bi.abs()
    // To avoid a possible division by zero
    if (val1.signum() == 0) {
      return val2
    } else if (val2.signum() == 0) {
      return val1
    }

    // Optimization for small operands
    // (op2.bitLength() < 64) and (op1.bitLength() < 64)
    if (((val1.numberLength == 1) || ((val1.numberLength == 2) && (val1.digits(1) > 0))) &&
      (val2.numberLength == 1 || (val2.numberLength == 2 && val2.digits(1) > 0))) {
      return BigInteger.valueOf(Division.gcdBinary(val1.longValue(), val2.longValue()))
    }
    Division.gcdBinary(val1.copy(), val2.copy())
  }

  /**
   * Returns the position of the lowest set bit in the two's complement
   * representation of this {@code BigInteger}. If all bits are zero (this=0)
   * then -1 is returned as result.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @return position of lowest bit if {@code this != 0}, {@code -1} otherwise
   */
  def getLowestSetBit(): Int = {
    if (sign == 0) {
      return -1
    }
    // (sign != 0) implies that exists some non zero digit
    val i = getFirstNonzeroDigit
    (i << 5) + java.lang.Integer.numberOfTrailingZeros(digits(i))
  }

  /**
   * Returns a hash code for this {@code BigInteger}.
   *
   * @return hash code for {@code this}.
   */
  override def hashCode(): Int = {
    if (_hashCode != 0) {
      return _hashCode
    }
    for (i ← 0 until digits.length) {
      _hashCode = _hashCode * 33 + (digits(i) & 0xffffffff)
    }
    _hashCode = _hashCode * sign
    _hashCode
  }


  /**
   * Returns this {@code BigInteger} as an int value. If {@code this} is too big
   * to be represented as an int, then {@code this} % 2^32 is returned.
   *
   * @return this {@code BigInteger} as an int value.
   */
  override def intValue(): Int = sign * digits(0)

  /**
   * Tests whether this {@code BigInteger} is probably prime. If {@code true} is
   * returned, then this is prime with a probability beyond (1-1/2^certainty).
   * If {@code false} is returned, then this is definitely composite. If the
   * argument {@code certainty} <= 0, then this method returns true.
   *
   * @param certainty tolerated primality uncertainty.
   * @return {@code true}, if {@code this} is probably prime, {@code false}
   *         otherwise.
   */
  def isProbablePrime(certainty: Int): Boolean = {
    Primality.isProbablePrime(abs(), certainty)
  }


  /**
   * Returns this {@code BigInteger} as an long value. If {@code this} is too
   * big to be represented as an long, then {@code this} % 2^64 is returned.
   *
   * @return this {@code BigInteger} as a long value.
   */
  override def longValue(): Long = {
    val value = if (numberLength > 1) (digits(1).toLong << 32) | (digits(0) & 0xFFFFFFFFL) else digits(0) & 0xFFFFFFFFL
    sign * value
  }


  /**
   * Returns the maximum of this {@code BigInteger} and {@code val}.
   *
   * @param bi value to be used to compute the maximum with {@code this}
   * @return {@code max(this, val)}
   * @throws NullPointerException if {@code val == null}
   */
  def max(bi: BigInteger): BigInteger = {
    if (this.compareTo(bi) == GREATER) this else bi
  }

  /**
   * Returns the minimum of this {@code BigInteger} and {@code val}.
   *
   * @param bi value to be used to compute the minimum with {@code this}.
   * @return {@code min(this, val)}.
   * @throws NullPointerException if {@code val == null}.
   */
  def min(bi: BigInteger): BigInteger = {
    if (this.compareTo(bi) == LESS) this else bi
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this mod m}. The
   * modulus {@code m} must be positive. The result is guaranteed to be in the
   * interval {@code [0, m)} (0 inclusive, m exclusive). The behavior of this
   * function is not equivalent to the behavior of the % operator defined for
   * the built-in {@code int}'s.
   *
   * @param m the modulus.
   * @return {@code this mod m}.
   * @throws NullPointerException if {@code m == null}.
   * @throws ArithmeticException if {@code m < 0}.
   */
  def mod(m: BigInteger): BigInteger = {
    if (m.sign <= 0) {
      throw new ArithmeticException("BigInteger: modulus not positive")
    }
    val rem = remainder(m)
    if (rem.sign < 0) rem.add(m) else rem
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code 1/this mod m}. The
   * modulus {@code m} must be positive. The result is guaranteed to be in the
   * interval {@code [0, m)} (0 inclusive, m exclusive). If {@code this} is not
   * relatively prime to m, then an exception is thrown.
   *
   * @param m the modulus.
   * @return {@code 1/this mod m}.
   * @throws NullPointerException if {@code m == null}
   * @throws ArithmeticException if {@code m < 0 or} if {@code this} is not
   *           relatively prime to {@code m}
   */
  def modInverse(m: BigInteger): BigInteger = {
    if (m.sign <= 0) {
      // math.18=BigInteger: modulus not positive
      throw new ArithmeticException("BigInteger: modulus not positive")
    }
    // If both are even, no inverse exists
    if (!(testBit(0) || m.testBit(0))) {
      // math.19=BigInteger not invertible.
      throw new ArithmeticException("BigInteger not invertible.")
    }
    if (m.isOne) {
      return ZERO
    }

    // From now on: (m > 1)
    var res = Division.modInverseMontgomery(abs().mod(m), m)
    if (res.sign == 0) {
      // math.19=BigInteger not invertible.
      throw new ArithmeticException("BigInteger not invertible.")
    }
    res = if ((sign < 0)) m.subtract(res) else res
    res
  }


  /**
   * Returns a new {@code BigInteger} whose value is {@code this^exponent mod m}
   * . The modulus {@code m} must be positive. The result is guaranteed to be in
   * the interval {@code [0, m)} (0 inclusive, m exclusive). If the exponent is
   * negative, then {@code this.modInverse(m)^(-exponent) mod m)} is computed.
   * The inverse of this only exists if {@code this} is relatively prime to m,
   * otherwise an exception is thrown.
   *
   * @param exponent the exponent.
   * @param m the modulus.
   * @return {@code this^exponent mod val}.
   * @throws NullPointerException if {@code m == null} or {@code exponent ==
   *           null}.
                    * @throws ArithmeticException if {@code m < 0} or if {@code exponent<0} and
   *           this is not relatively prime to {@code m}.
   */
  def modPow(exponent: BigInteger, m: BigInteger): BigInteger = {
    var _exponent = exponent
    if (m.sign <= 0) {
      throw new ArithmeticException("BigInteger: modulus not positive")
    }
    var base = this
    if (m.isOne | (_exponent.sign > 0 & base.sign == 0)) {
      return BigInteger.ZERO
    }
    if (base.sign == 0 && _exponent.sign == 0) {
      return BigInteger.ONE
    }
    if (_exponent.sign < 0) {
      base = modInverse(m)
      _exponent = _exponent.negate()
    }
    // From now on: (m > 0) and (exponent >= 0)
    var res = if (m.testBit(0)) Division.oddModPow(base.abs(), _exponent, m) else Division.evenModPow(base.abs(),
      _exponent, m)
    if ((base.sign < 0) && _exponent.testBit(0)) {
      res = m.subtract(BigInteger.ONE).multiply(res).mod(m)
    }
    res
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this * val}.
   *
   * @param val value to be multiplied with {@code this}.
   * @return {@code this * val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def multiply(bi: BigInteger): BigInteger = {
    if (bi.sign == 0) {
      return ZERO
    }
    if (sign == 0) {
      return ZERO
    }
    Multiplication.multiply(this, bi)
  }

  /**
   * Returns a new {@code BigInteger} whose value is the {@code -this}.
   *
   * @return {@code -this}.
   */
  def negate(): BigInteger = {
    if ((sign == 0)) this else new BigInteger(-sign, numberLength, digits)
  }

  /**
   * Returns the smallest integer x > {@code this} which is probably prime as a
   * {@code BigInteger} instance. The probability that the returned {@code
   * BigInteger} is prime is beyond (1-1/2^80).
   *
   * @return smallest integer > {@code this} which is robably prime.
   * @throws ArithmeticException if {@code this < 0}.
   */
  def nextProbablePrime(): BigInteger = {
    if (sign < 0) {
      throw new ArithmeticException("start < 0: " + this)
    }
    Primality.nextProbablePrime(this)
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code ~this}. The result
   * of this operation is {@code -this-1}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @return {@code ~this}.
   */
  def not(): BigInteger = Logical.not(this)


  /**
   * Returns a new {@code BigInteger} whose value is {@code this | val}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param bi value to be or'ed with {@code this}.
   * @return {@code this | val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def or(bi: BigInteger): BigInteger = Logical.or(this, bi)


  /**
   * Returns a new {@code BigInteger} whose value is {@code this ^ exp}.
   *
   * @param exp exponent to which {@code this} is raised.
   * @return {@code this ^ exp}.
   * @throws ArithmeticException if {@code exp < 0}.
   */
  def pow(exp: Int): BigInteger = {
    if (exp < 0) {
      throw new ArithmeticException("Negative exponent")
    }
    if (exp == 0) {
      return ONE
    } else if (exp == 1 || exp == ONE || exp == ZERO) {
      return this
    }

    // if even take out 2^x factor which we can
    // calculate by shifting.
    if (!testBit(0)) {
      var x = 1
      while (!testBit(x)) {
        x += 1
      }
      return getPowerOfTwo(x * exp).multiply(this.shiftRight(x).pow(exp))
    }
    Multiplication.pow(this, exp)
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this % divisor}.
   * Regarding signs this methods has the same behavior as the % operator on
   * int's, i.e. the sign of the remainder is the same as the sign of this.
   *
   * @param divisor value by which {@code this} is divided.
   * @return {@code this % divisor}.
   * @throws NullPointerException if {@code divisor == null}.
   * @throws ArithmeticException if {@code divisor == 0}.
   */
  def remainder(divisor: BigInteger): BigInteger = {
    if (divisor.sign == 0) {
      throw new ArithmeticException("BigInteger divide by zero")
    }
    val thisLen = numberLength
    val divisorLen = divisor.numberLength
    if ((if (thisLen != divisorLen) if ((thisLen > divisorLen)) 1 else -1 else Elementary.compareArrays(digits,
      divisor.digits, thisLen)) ==
      LESS) {
      return this
    }
    val resLength = divisorLen
    var resDigits = new Array[Int](resLength)
    if (resLength == 1) {
      resDigits(0) = Division.remainderArrayByInt(digits, thisLen, divisor.digits(0))
    } else {
      val qLen = thisLen - divisorLen + 1
      resDigits = Division.divide(null, qLen, digits, thisLen, divisor.digits, divisorLen)
    }
    val result = new BigInteger(sign, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /**
   * Returns a new {@code BigInteger} which has the same binary representation
   * as {@code this} but with the bit at position n set. The result is
   * equivalent to {@code this | 2^n}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param n position where the bit in {@code this} has to be set.
   * @return {@code this | 2^n}.
   * @throws ArithmeticException if {@code n < 0}.
   */
  def setBit(n: Int): BigInteger = {
    if (!testBit(n)) {
      return BitLevel.flipBit(this, n)
    }
    this
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this << n}. The
   * result is equivalent to {@code this * 2^n} if n >= 0. The shift distance
   * may be negative which means that {@code this} is shifted right. The result
   * then corresponds to {@code floor(this / 2^(-n))}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method on negative values is not
   * recommended as the current implementation is not efficient.
   *
   * @param n shift distance.
   * @return {@code this << n} if {@code n >= 0}; {@code this >> (-n)}.
   *         otherwise
   */
  def shiftLeft(n: Int): BigInteger = {
    if ((n == 0) || (sign == 0)) {
      return this
    }
    if ((n > 0)) BitLevel.shiftLeft(this, n) else BitLevel.shiftRight(this, -n)
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this >> n}. For
   * negative arguments, the result is also negative. The shift distance may be
   * negative which means that {@code this} is shifted left.
   * <p>
   * <b>Implementation Note:</b> Usage of this method on negative values is not
   * recommended as the current implementation is not efficient.
   *
   * @param n shift distance
   * @return {@code this >> n} if {@code n >= 0}; {@code this << (-n)} otherwise
   */
  def shiftRight(n: Int): BigInteger = {
    if ((n == 0) || (sign == 0)) {
      return this
    }
    if ((n > 0)) BitLevel.shiftRight(this, n) else BitLevel.shiftLeft(this, -n)
  }

  /**
   * Returns the sign of this {@code BigInteger}.
   *
   * @return {@code -1} if {@code this < 0}, {@code 0} if {@code this == 0},
   *         {@code 1} if {@code this > 0}.
   */
  def signum(): Int = sign

  /**
   * Returns a new {@code BigInteger} whose value is {@code this - val}.
   *
   * @param bi value to be subtracted from {@code this}.
   * @return {@code this - val}.
   * @throws NullPointerException if {@code val == null}.
   */
  def subtract(bi: BigInteger): BigInteger = Elementary.subtract(this, bi)


  /**
   * Tests whether the bit at position n in {@code this} is set. The result is
   * equivalent to {@code this & (2^n) != 0}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param n position where the bit in {@code this} has to be inspected.
   * @return {@code this & (2^n) != 0}.
   * @throws ArithmeticException if {@code n < 0}.
   */
  def testBit(n: Int): Boolean = {
    var _n = n
    if (_n == 0) {
      return (digits(0) & 1) != 0
    }
    if (_n < 0) {
      throw new ArithmeticException("Negative bit address")
    }
    val intCount = _n >> 5
    if (intCount >= numberLength) {
      return sign < 0
    }
    var digit = digits(intCount)
    _n = 1 << (_n & 31)
    if (sign < 0) {
      val firstNonZeroDigit = getFirstNonzeroDigit
      if (intCount < firstNonZeroDigit) {
        return false
      } else digit = if (firstNonZeroDigit == intCount) -digit else ~digit
    }
    (digit & _n) != 0
  }

  /**
   * Returns the two's complement representation of this BigInteger in a byte
   * array.
   *
   * @return two's complement representation of {@code this}.
   */
  def toByteArray(): Array[Byte] = {
    if (this.sign == 0) {
      return Array[Byte](0)
    }
    val temp: BigInteger = this
    val bitLen = bitLength()
    val iThis = getFirstNonzeroDigit
    var bytesLen = (bitLen >> 3) + 1
    /*
     * Puts the little-endian int array representing the magnitude of this
     * BigInteger into the big-endian byte array.
     */
    val bytes = new Array[Byte](bytesLen)
    var firstByteNumber = 0
    var highBytes: Int = 0
    var digitIndex = 0
    var bytesInInteger = 4
    var digit: Int = 0
    var hB: Int = 0

    if (bytesLen - (numberLength << 2) == 1) {
      bytes(0) = (if (sign < 0) -1 else 0).toByte
      highBytes = 4
      firstByteNumber += 1
    } else {
      hB = bytesLen & 3
      highBytes = if (hB == 0) 4 else hB
    }
    digitIndex = iThis
    bytesLen -= iThis << 2
    if (sign < 0) {
      digit = -temp.digits(digitIndex)
      digitIndex += 1
      if (digitIndex == numberLength) {
        bytesInInteger = highBytes
      }
      var i = 0
      while (i < bytesInInteger) {
        bytesLen -=1
        bytes(bytesLen) = digit.toByte
        i += 1
        digit >>= 8
      }
      while (bytesLen > firstByteNumber) {
        digit = ~temp.digits(digitIndex)
        digitIndex += 1
        if (digitIndex == numberLength) {
          bytesInInteger = highBytes
        }
        var i = 0
        while (i < bytesInInteger) {
          bytesLen -=1
          bytes(bytesLen) = digit.toByte
          i += 1
          digit >>= 8
        }
      }
    } else {
      while (bytesLen > firstByteNumber) {
        digit = temp.digits(digitIndex)
        digitIndex += 1
        if (digitIndex == numberLength) {
          bytesInInteger = highBytes
        }
        var i = 0
        while (i < bytesInInteger) {
          bytesLen -=1
          bytes(bytesLen) = digit.toByte
          i += 1
          digit >>= 8
        }
      }
    }
    bytes
  }

  /**
   * Returns a string representation of this {@code BigInteger} in decimal form.
   *
   * @return a string representation of {@code this} in decimal form.
   */
  override def toString(): String = {
    Conversion.toDecimalScaledString(this, 0)
  }

  /**
   * Returns a string containing a string representation of this {@code
   * BigInteger} with base radix. If {@code radix} is less than
   * {@link Character#MIN_RADIX} or greater than {@link Character#MAX_RADIX}
   * then a decimal representation is returned. The characters of the string
   * representation are generated with method {@link Character#forDigit}.
   *
   * @param radix base to be used for the string representation.
   * @return a string representation of this with radix 10.
   */
  def toString(radix: Int): String = {
    Conversion.bigInteger2String(this, radix)
  }

  /**
   * Returns a new {@code BigInteger} whose value is {@code this ^ val}.
   * <p>
   * <b>Implementation Note:</b> Usage of this method is not recommended as the
   * current implementation is not efficient.
   *
   * @param bi value to be xor'ed with {@code this}
   * @return {@code this ^ val}
   * @throws NullPointerException if {@code val == null}
   */
  def xor(bi: BigInteger): BigInteger = Logical.xor(this, bi)

  /**
   * Returns a copy of the current instance to achieve immutability
   */
  def copy(): BigInteger = {
    val copyDigits = new Array[Int](numberLength)
    System.arraycopy(digits, 0, copyDigits, 0, numberLength)
    new BigInteger(sign, numberLength, copyDigits)
  }

  def cutOffLeadingZeroes(): Unit = {
    while ((numberLength > 0) && (digits({numberLength  -= 1;numberLength}) == 0)) {
    }

    if (digits(numberLength) == 0) {
      sign = 0
    }
    numberLength += 1
  }

  def equalsArrays(b: Array[Int]): Boolean = {
    var i: Int = 0
    i = numberLength - 1
    while ((i >= 0) && (digits(i) == b(i))) {
      i -= 1
    }
    i < 0
  }

  def getFirstNonzeroDigit(): Int = {
    if (firstNonzeroDigit == -2) {
      var i: Int = 0
      if (this.sign == 0) {
        i = -1
      } else {
        i = 0
        while (digits(i) == 0) {
          i += 1
        }
      }
      firstNonzeroDigit = i
    }
    firstNonzeroDigit
  }

  /**
   * Tests if {@code this.abs()} is equals to {@code ONE}.
   */
  def isOne(): Boolean = {
    (numberLength == 1) && (digits(0) == 1)
  }

  def shiftLeftOneBit(): BigInteger = {
    if (sign == 0) this else BitLevel.shiftLeftOneBit(this)
  }

  def unCache(): Unit = {
    firstNonzeroDigit = -2
  }

  /**
   * Puts a big-endian byte array into a little-endian applying two complement.
   */
  private def putBytesNegativeToIntegers(byteValues: Array[Byte]): Unit = {
    var bytesLen = byteValues.length
    val highBytes = bytesLen & 3
    numberLength = (bytesLen >> 2) + (if (highBytes == 0) 0 else 1)
    digits = new Array[Int](numberLength)
    var i = 0
    // Setting the sign
    digits(numberLength - 1) = -1
    // Put bytes to the int array starting from the end of the byte array
    def loop: Unit = while (bytesLen > highBytes) {
      digits(i) = (byteValues(bytesLen - 1) & 0xFF)       |
                  (byteValues(bytesLen - 2) & 0xFF) << 8  |
                  (byteValues(bytesLen - 3) & 0xFF) << 16 |
                  (byteValues(bytesLen - 4) & 0xFF) << 24
      bytesLen -= 4
      if (digits(i) != 0) {
        digits(i) = -digits(i)
        firstNonzeroDigit = i
        i += 1
        while (bytesLen > highBytes) {
          digits(i) = (byteValues(bytesLen - 1) & 0xFF)       |
            (byteValues(bytesLen - 2) & 0xFF) << 8  |
            (byteValues(bytesLen - 3) & 0xFF) << 16 |
            (byteValues(bytesLen - 4) & 0xFF) << 24
          bytesLen -= 4
          digits(i) = ~digits(i)
          i += 1
        }
        //break changed to def
        return
      }
      i += 1
    }
    loop

    if (highBytes != 0) {
      // Put the first bytes in the highest element of the int array
      if (firstNonzeroDigit != -2) {
        for (j ← 0 until bytesLen) {
          digits(i) = (digits(i) << 8) | (byteValues(j) & 0xFF)
        }
        digits(i) = ~digits(i)
      } else {
        for (j ← 0 until bytesLen) {
          digits(i) = (digits(i) << 8) | (byteValues(j) & 0xFF)
        }
        digits(i) = -digits(i)
      }
    }
  }

  /**
   * Puts a big-endian byte array into a little-endian int array.
   */
  private def putBytesPositiveToIntegers(byteValues: Array[Byte]): Unit = {
    var bytesLen = byteValues.length
    val highBytes = bytesLen & 3
    numberLength = (bytesLen >> 2) + (if (highBytes == 0) 0 else 1)
    digits = new Array[Int](numberLength)

    // Put bytes to the int array starting from the end of the byte array
    var i = 0
    while (bytesLen > highBytes) {
      digits(i) = (byteValues(bytesLen - 1) & 0xFF)       |
                  (byteValues(bytesLen - 2) & 0xFF) << 8  |
                  (byteValues(bytesLen - 3) & 0xFF) << 16 |
                  (byteValues(bytesLen - 4) & 0xFF) << 24
      bytesLen = bytesLen  -4
      i += 1
    }
    // Put the first bytes in the highest element of the int array
    for (j ← 0 until bytesLen) {
      digits(i) = (digits(i) << 8) | (byteValues(j) & 0xFF)
    }
  }

  /**
   * @see BigInteger#BigInteger(String, int)
   */
  private def setFromString(s: String, radix: Int): Unit = {
    var _sign: Int = 0
    var _digits: Array[Int] = Array()
    var _numberLength: Int = 0
    var stringLength = s.length
    var startChar: Int = 0
    val endChar = stringLength

    if (s.charAt(0) == '-') {
      _sign = -1
      startChar = 1
      stringLength -= 1
    } else {
      _sign = 1
      startChar = 0
    }
    /*
     * We use the following algorithm: split a string into portions of n
     * characters and convert each portion to an integer according to the radix.
     * Then convert an exp(radix, n) based number to binary using the
     * multiplication method. See D. Knuth, The Art of Computer Programming,
     * vol. 2.
     */
    val charsPerInt = Conversion.digitFitInInt(radix)
    var bigRadixDigitsLength = stringLength / charsPerInt
    val topChars = stringLength % charsPerInt
    if (topChars != 0) {
      bigRadixDigitsLength += 1
    }
    _digits = new Array[Int](bigRadixDigitsLength)
    val bigRadix = Conversion.bigRadices(radix - 2)
    var digitIndex = 0
    var substrEnd = startChar + (if (topChars == 0) charsPerInt else topChars)
    var newDigit: Int = 0
    var substrStart = startChar
    while (substrStart < endChar) {
      val bigRadixDigit = java.lang.Integer.parseInt(s.substring(substrStart, substrEnd), radix)
      newDigit = Multiplication.multiplyByInt(_digits, digitIndex, bigRadix)
      newDigit += Elementary.inplaceAdd(_digits, digitIndex, bigRadixDigit)
      _digits(digitIndex) = newDigit
      digitIndex += 1
      substrStart = substrEnd
      substrEnd = substrStart + charsPerInt
    }
    _numberLength = digitIndex
    this.sign = _sign
    this.numberLength = _numberLength
    this.digits = _digits
    this.cutOffLeadingZeroes()
  }
}

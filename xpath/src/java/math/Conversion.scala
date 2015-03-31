// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.

/*
 *  Ported by Alistair Johnson from  https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/Conversion.java
 */

package java.math


/**
 * Static library that provides {@link BigInteger} base conversion from/to any
 * integer represented in a {@link java.lang.String} Object.
 */
object Conversion {

  /**
   * Holds the maximal exponent for each radix, so that radix<sup>digitFitInInt[radix]</sup>
   * fit in an {@code int} (32 bits).
   */
  val digitFitInInt = Array[Int](
    -1, -1, 31, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)

  /**
   * bigRadices values are precomputed maximal powers of radices (integer
   * numbers from 2 to 36) that fit into unsigned int (32 bits). bigRadices[0] =
   * 2 ^ 31, bigRadices[8] = 10 ^ 9, etc.
   */
  val bigRadices = Array[Int](
    -2147483648, 1162261467, 1073741824, 1220703125, 362797056, 1977326743,
    1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
    1475789056, 170859375, 268435456, 410338673, 612220032, 893871739,
    1280000000, 1801088541, 113379904, 148035889, 191102976, 244140625,
    308915776, 387420489, 481890304, 594823321, 729000000, 887503681,
    1073741824, 1291467969, 1544804416, 1838265625, 60466176
  )

  /** @see BigInteger#toString(int) */
  def bigInteger2String(bi: BigInteger, radix: Int): String = {
    val sign = bi.sign
    val numberLength = bi.numberLength
    val digits = bi.digits
    if (sign == 0) {
      return "0"
    }
    if (numberLength == 1) {
      val highDigit = digits(numberLength - 1)
      var v = highDigit & 0xFFFFFFFFL
      if (sign < 0) {
        v = -v
      }
      return java.lang.Long.toString(v, radix)
    }
    if ((radix == 10) || (radix < java.lang.Character.MIN_RADIX) ||
      (radix > java.lang.Character.MAX_RADIX)) {
      return bi.toString
    }
    var bitsForRadixDigit: Double = 0.0
    bitsForRadixDigit = Math.log(radix) / Math.log(2)
    val resLengthInChars = (bi.abs().bitLength() / bitsForRadixDigit + (if (sign < 0) 1 else 0)).toInt +
      1
    val result = new Array[Char](resLengthInChars)
    var currentChar = resLengthInChars
    var resDigit: Int = 0
    if (radix != 16) {
      val temp = new Array[Int](numberLength)
      System.arraycopy(digits, 0, temp, 0, numberLength)
      var tempLen = numberLength
      val charsPerInt = digitFitInInt(radix)
      var i: Int = 0
      val bigRadix = bigRadices(radix - 2)

      def loop: Unit = while (true) {
        resDigit = Division.divideArrayByInt(temp, temp, tempLen, bigRadix)
        val previous = currentChar
        do {
          currentChar -= 1
          result(currentChar) = java.lang.Character.forDigit(resDigit % radix, radix)
        } while ( ( {
          resDigit /= radix; resDigit} != 0)  && (currentChar != 0))

        val delta = charsPerInt - previous + currentChar
        i = 0
        while (i < delta && currentChar > 0) {
          currentChar -= 1
          result(currentChar) = '0'
          i += 1
        }
        i = tempLen - 1
        while ((i > 0) && (temp(i) == 0)) {

          i -= 1
        }
        tempLen = i + 1
        if ((tempLen == 1) && (temp(0) == 0)) {
          //break changed to def
          return
        }
      }
      loop

    } else {
      for (i <- 0 until numberLength) {
        var j = 0
        while ((j < 8) && (currentChar > 0)) {
          resDigit = digits(i) >> (j << 2) & 0xf
          currentChar -= 1
          result(currentChar) = java.lang.Character.forDigit(resDigit, 16)
          j += 1
        }
      }
    }
    while (result(currentChar) == '0') {
      currentChar += 1
    }
    if (sign == -1) {
      currentChar -= 1
      result(currentChar) = '-'
    }
    new String(result, currentChar, resLengthInChars - currentChar)
  }


  /**
   * Builds the correspondent {@code String} representation of {@code val} being
   * scaled by {@code scale}.
   *
   * @see BigInteger#toString()
   * @see BigDecimal#toString()
   */
  def toDecimalScaledString(bi: BigInteger, scale: Int): String = {

    val sign:Int = bi.sign
    val numberLength:Int = bi.numberLength
    val digits:Array[Int] = bi.digits
    var resLengthInChars: Int = 0
    var currentChar: Int = 0

    if (sign == 0) scale match {
      case 0 => return "0"
      case 1 => return "0.0"
      case 2 => return "0.00"
      case 3 => return "0.000"
      case 4 => return "0.0000"
      case 5 => return "0.00000"
      case 6 => return "0.000000"
      case _ =>
        val result1 = new java.lang.StringBuilder()
        if (scale < 0) {
          result1.append("0E+")
        } else {
          result1.append("0E")
        }
        result1.append(-scale)
        return result1.toString

    }
    // one 32-bit unsigned value may contains 10 decimal digits
    resLengthInChars = numberLength * 10 + 1 + 7
    // Explanation why +1+7:
    // +1 - one char for sign if needed.
    // +7 - For "special case 2" (see below) we have 7 free chars for
    // inserting necessary scaled digits.
    val result: Array[Char] = new Array[Char](resLengthInChars + 1)
    // allocated [resLengthInChars+1] characters.
    // a free latest character may be used for "special case 1" (see below)
    currentChar = resLengthInChars
    if (numberLength == 1) {
      val highDigit = digits(0)
      if (highDigit < 0) {
        var v: Long = highDigit & 0xFFFFFFFFL
        do {
          val prev = v
          v /= 10
          currentChar -= 1
          result(currentChar) = (0x0030 + (prev - v * 10).toInt).toChar
        } while (v != 0);
      } else {
        var v:Int = highDigit
        do {
          val prev = v
          v /= 10
          currentChar -= 1
          result(currentChar) = (0x0030 + (prev - v * 10)).toChar
        } while (v != 0)
      }
    } else {
      val temp = new Array[Int](numberLength)
      var tempLen = numberLength
      System.arraycopy(digits, 0, temp, 0, tempLen)

      def BIG_LOOP:Unit = while (true) {
        // divide the array of digits by bigRadix and convert
        // remainders
        // to characters collecting them in the char array
        var result11: Long = 0
        var i1: Int = tempLen - 1
        while (i1 >= 0) {
          val temp1: Long = (result11 << 32) + (temp(i1) & 0xFFFFFFFFL)
          val res: Long = divideLongByBillion(temp1)
          temp(i1) = res.toInt
          result11 = (res >> 32).toInt
          i1 -= 1
        }
        var resDigit = result11.toInt
        val previous = currentChar
        do {
          currentChar -= 1
          result(currentChar) = (0x0030 + (resDigit % 10)).toChar
        } while (( {
          resDigit /= 10;resDigit} != 0) && (currentChar != 0));
        val delta = 9 - previous + currentChar
        var i = 0
        while ((i < delta) && (currentChar > 0)) {
          currentChar -= 1
          result(currentChar) = '0'
          i += 1
        }
        var j = tempLen - 1
        while (temp(j) == 0) {
          if (j == 0) {
            return
          }
          j -= 1
        }
        tempLen = j + 1
      }
      BIG_LOOP
      while (result(currentChar) == '0') {
        currentChar += 1
      }
    }

    val negNumber = sign < 0
    val exponent = resLengthInChars - currentChar - scale - 1
    if (scale == 0) {
      if (negNumber) {
        currentChar -= 1
        result(currentChar) = '-'
      }
      return new String(result, currentChar, resLengthInChars - currentChar)
    }
    if ((scale > 0) && (exponent >= -6)) {
      if (exponent >= 0) {
        // special case 1
        var insertPoint = currentChar + exponent
        var j = resLengthInChars - 1
        while (j >= insertPoint) {
          result(j + 1) = result(j)
          j -= 1
        }
        insertPoint += 1
        result(insertPoint) = '.'
        if (negNumber) {
          currentChar -= 1
          result(currentChar) = '-'
        }
        return new String(result, currentChar, resLengthInChars - currentChar + 1)
      }
      // special case 2
      for (j <- 2 until -exponent + 1) {
        currentChar -= 1
        result(currentChar) = '0'
      }
      currentChar -= 1
      result(currentChar) = '.'
      currentChar -= 1
      result(currentChar) = '0'
      if (negNumber) {
        currentChar -= 1
        result(currentChar) = '-'
      }
      return new String(result, currentChar, resLengthInChars - currentChar)
    }
    val startPoint = currentChar + 1
    val endPoint = resLengthInChars
    val result1 = new java.lang.StringBuilder(16 + endPoint - startPoint)
    if (negNumber) {
      result1.append('-')
    }
    if (endPoint - startPoint >= 1) {
      result1.append(result(currentChar))
      result1.append('.')
      result1.append(result, currentChar + 1, resLengthInChars - currentChar - 1)
    } else {
      result1.append(result, currentChar, resLengthInChars - currentChar)
    }
    result1.append('E')
    if (exponent > 0) {
      result1.append('+')
    }
    result1.append(java.lang.Integer.toString(exponent))
    result1.toString
  }

  /* can process only 32-bit numbers */
  def toDecimalScaledString(value: Long, scale: Int): String = {
    var _value = value
    var resLengthInChars: Int = 0
    var currentChar: Int = 0
    var result: Array[Char] = null
    val negNumber = _value < 0
    if (negNumber) {
      _value = -_value
    }
    if (_value == 0) scale match {
      case 0 => return "0"
      case 1 => return "0.0"
      case 2 => return "0.00"
      case 3 => return "0.000"
      case 4 => return "0.0000"
      case 5 => return "0.00000"
      case 6 => return "0.000000"
      case _ =>
        val result1 = new java.lang.StringBuilder()
        if (scale < 0) {
          result1.append("0E+")
        } else {
          result1.append("0E")
        }
        result1.append(if (scale == java.lang.Integer.MIN_VALUE) "2147483648" else java.lang.Integer.toString(-scale))
        return result1.toString

    }
    // one 32-bit unsigned value may contains 10 decimal digits
    resLengthInChars = 18
    // Explanation why +1+7:
    // +1 - one char for sign if needed.
    // +7 - For "special case 2" (see below) we have 7 free chars for
    //  inserting necessary scaled digits.
    result = new Array[Char](resLengthInChars + 1)
    //  Allocated [resLengthInChars+1] characters.
    // a free latest character may be used for "special case 1" (see below)
    currentChar = resLengthInChars

    var v: Long = _value
    do {
      val prev = v
      v /= 10
      currentChar -= 1
      result(currentChar) = (0x0030 + (prev - v * 10)).toChar
    } while (v != 0)

    val exponent:Long = resLengthInChars.toLong - currentChar.toLong - scale -
      1L
    if (scale == 0) {
      if (negNumber) {
        currentChar -= 1
        result(currentChar) = '-'
      }
      return new String(result, currentChar, resLengthInChars - currentChar)
    }
    if (scale > 0 && exponent >= -6) {
      if (exponent >= 0) {
        // special case 1
        var insertPoint = currentChar + exponent.toInt
        var j = resLengthInChars - 1
        while (j >= insertPoint) {
          result(j + 1) = result(j)
          j -= 1
        }
        insertPoint += 1
        result(insertPoint) = '.'
        if (negNumber) {
          currentChar -= 1
          result(currentChar) = '-'
        }
        return new String(result, currentChar, resLengthInChars - currentChar + 1)
      }
      // special case 2
      for (j <- 2 until (-exponent + 1).toInt) {
        currentChar -= 1
        result(currentChar) = '0'
      }
      currentChar -= 1
      result(currentChar) = '.'
      currentChar -= 1
      result(currentChar) = '0'
      if (negNumber) {
        result(currentChar) = '-'
      }
      return new String(result, currentChar, resLengthInChars - currentChar)
    }
    val startPoint = currentChar + 1
    val endPoint = resLengthInChars
    val result1 = new java.lang.StringBuilder(16 + endPoint - startPoint)
    if (negNumber) {
      result1.append('-')
    }
    if (endPoint - startPoint >= 1) {
      result1.append(result(currentChar))
      result1.append('.')
      result1.append(result, currentChar + 1, resLengthInChars - currentChar - 1)
    } else {
      result1.append(result, currentChar, resLengthInChars - currentChar)
    }
    result1.append('E')
    if (exponent > 0) {
      result1.append('+')
    }
    result1.append(java.lang.Long.toString(exponent))
    result1.toString
  }

  def divideLongByBillion(a: Long): Long = {
    var quot: Long = 0l
    var rem: Long = 0l

    if (a >= 0) {
      val bLong = 1000000000L
      quot = a / bLong
      rem = a % bLong
    } else {
      /*
       * Make the dividend positive shifting it right by 1 bit then get
       * the quotient an remainder and correct them properly
       */
      val aPos: Long = a >>> 1
      val bPos: Long = 1000000000L >>> 1
      quot = aPos / bPos
      rem = aPos % bPos
      rem = (rem << 1) + (a & 1)
    }
    (rem << 32) | (quot & 0xFFFFFFFFL)
  }

  def bigInteger2Double(bi: BigInteger): Double = {
    if ((bi.numberLength < 2) ||
      ((bi.numberLength == 2) && (bi.digits(1) > 0))) {
      return bi.longValue()
    }
    if (bi.numberLength > 32) {
      return if ((bi.sign > 0)) java.lang.Double.POSITIVE_INFINITY else java.lang.Double.NEGATIVE_INFINITY
    }
    val bitLen = bi.abs().bitLength()
    var exponent:Long = bitLen - 1
    val delta = bitLen - 54
    val lVal = bi.abs().shiftRight(delta).longValue()
    var mantissa = lVal & 0x1FFFFFFFFFFFFFL
    if (exponent == 1023) {
      if (mantissa == 0X1FFFFFFFFFFFFFL) {
        return if ((bi.sign > 0)) java.lang.Double.POSITIVE_INFINITY else java.lang.Double.NEGATIVE_INFINITY
      }
      if (mantissa == 0x1FFFFFFFFFFFFEL) {
        return if ((bi.sign > 0)) java.lang.Double.MAX_VALUE else -java.lang.Double.MAX_VALUE
      }
    }
    if (((mantissa & 1) == 1) &&
      (((mantissa & 2) == 2) || BitLevel.nonZeroDroppedBits(delta, bi.digits))) {
      mantissa += 2
    }
    mantissa >>= 1
    val resSign = if (bi.sign < 0) 0x8000000000000000L else 0
    exponent = ((1023 + exponent) << 52) & 0x7FF0000000000000L
    val result = resSign | exponent | mantissa
    java.lang.Double.longBitsToDouble(result)
  }

}

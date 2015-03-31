/*
 *  Ported by Alistair Johnson from  https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/math/Elementary.java
 */

package java.math


/**
 * Object that provides the basic arithmetic mutable operations for
 * {@link BigInteger}. The operations provided are listed below. <ul
 * type="circle"> <li>Addition.</li> <li>Subtraction.</li> <li>Comparison.</li>
 * </ul> In addition to this, some <i><b>Inplace</b></i> (mutable) methods are
 * provided.
 */
object Elementary {

  /**
   * @see BigInteger#add(BigInteger) .
   * @param op1
   * @param op2
   * @return
   */
  def add(op1: BigInteger, op2: BigInteger): BigInteger = {
    var resDigits: Array[Int] = null
    var resSign: Int = 0
    val op1Sign = op1.sign
    val op2Sign = op2.sign
    if (op1Sign == 0) {
      return op2
    }
    if (op2Sign == 0) {
      return op1
    }
    val op1Len:Int = op1.numberLength
    val op2Len:Int = op2.numberLength
    if ( (op1Len + op2Len) == 2) {
      val a: Long = op1.digits(0) & 0xFFFFFFFFL
      val b: Long = op2.digits(0) & 0xFFFFFFFFL
      var res: Long = 0l
      var valueLo: Int = 0
      var valueHi: Int = 0
      if (op1Sign == op2Sign) {
        res = a + b
        valueLo = res.toInt
        valueHi = (res >>> 32).toInt
        return if (valueHi == 0) new BigInteger(op1Sign, valueLo) else new BigInteger(op1Sign, 2,
          Array(valueLo, valueHi))
      }
      return BigInteger.valueOf(if (op1Sign < 0) (b - a) else (a - b))
    } else if (op1Sign == op2Sign) {
      resSign = op1Sign
      // an augend should not be shorter than addend
      resDigits = if ((op1Len >= op2Len)) add(op1.digits, op1Len, op2.digits, op2Len) else add(op2.digits,
        op2Len, op1.digits, op1Len)
    } else {// signs are different
      val cmp = (if ((op1Len != op2Len)) (if ((op1Len > op2Len)) 1 else -1) else compareArrays(op1.digits,
        op2.digits, op1Len))
      if (cmp == BigInteger.EQUALS) {
        return BigInteger.ZERO
      }
      // a minuend should not be shorter than subtrahend
      if (cmp == BigInteger.GREATER) {
        resSign = op1Sign
        resDigits = subtract(op1.digits, op1Len, op2.digits, op2Len)
      } else {
        resSign = op2Sign
        resDigits = subtract(op2.digits, op2Len, op1.digits, op1Len)
      }
    }
    val res = new BigInteger(resSign, resDigits.length, resDigits)
    res.cutOffLeadingZeroes()
    res
  }

  def compareArrays(a: Array[Int], b: Array[Int], size: Int): Int = {
    var i: Int = 0
    i = size - 1
    while ((i >= 0) && (a(i) == b(i))) {
      i -= 1
    }
    (if ((i < 0)) BigInteger.EQUALS else if ((a(i) & 0xFFFFFFFFL) < (b(i) & 0xFFFFFFFFL)) BigInteger.LESS else BigInteger.GREATER)
  }


  /**
   * Same as @link #inplaceAdd(BigInteger, BigInteger), but without the
   * restriction of non-positive values.
   *
   * @param op1 any number
   * @param op2 any number
   */
  def completeInPlaceAdd(op1: BigInteger, op2: BigInteger) {
    if (op1.sign == 0) {
      require( op2.numberLength >= 0)
      require( op2.numberLength <= op2.digits.length)
      require( op2.numberLength <= op1.digits.length )
      System.arraycopy(op2.digits, 0, op1.digits, 0, op2.numberLength)
    } else if (op2.sign == 0) {
      return
    } else if (op1.sign == op2.sign) {
      add(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
    } else {
      val sign = unsignedArraysCompare(op1.digits, op2.digits, op1.numberLength, op2.numberLength)
      if (sign > 0) {
        subtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
      } else {
        inverseSubtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
        op1.sign = -op1.sign
      }
    }
    op1.numberLength = Math.max(op1.numberLength, op2.numberLength) + 1
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /**
   * Same as @link #inplaceSubtract(BigInteger, BigInteger), but without the
   * restriction of non-positive values.
   *
   * @param op1 should have enough space to save the result
   * @param op2
   */
  def completeInPlaceSubtract(op1: BigInteger, op2: BigInteger) {
    val resultSign = op1.compareTo(op2)
    if (op1.sign == 0) {
      System.arraycopy(op2.digits, 0, op1.digits, 0, op2.numberLength)
      op1.sign = -op2.sign
    } else if (op1.sign != op2.sign) {
      add(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
      op1.sign = resultSign
    } else {
      val sign = unsignedArraysCompare(op1.digits, op2.digits, op1.numberLength, op2.numberLength)
      if (sign > 0) {
        subtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
      } else {
        inverseSubtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
        op1.sign = -op1.sign
      }
    }
    op1.numberLength = Math.max(op1.numberLength, op2.numberLength) + 1
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /**
   * Performs {@code op1 += op2}. {@code op1} must have enough place to store
   * the result (i.e. {@code op1.bitLength() >= op2.bitLength()}). Both should
   * be positive (i.e. {@code op1 >= op2}).
   *
   * @param op1 the input minuend, and the output result.
   * @param op2 the addend
   */
  def inplaceAdd(op1: BigInteger, op2: BigInteger) {
    add(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
    op1.numberLength = Math.min(Math.max(op1.numberLength, op2.numberLength) + 1, op1.digits.length)
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /**
   * Performs: {@code op1 += addend}. The number must to have place to hold a
   * possible carry.
   */
  def inplaceAdd(op1: BigInteger, addend: Int) {
    val carry = inplaceAdd(op1.digits, op1.numberLength, addend)
    if (carry == 1) {
      op1.digits(op1.numberLength) = 1
      op1.numberLength += 1
    }
    op1.unCache()
  }

  /**
   * Adds an integer value to the array of integers remembering carry.
   *
   * @return a possible generated carry (0 or 1)
   */
  def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    var carry:Long = addend & 0xFFFFFFFFL
    var i = 0
    while ((carry != 0) && (i < aSize)) {
      carry += (a(i) & 0xFFFFFFFFL)
      a(i) = carry.toInt
      carry >>= 32
      i += 1
    }
    carry.toInt
  }

  /**
   * Performs {@code op1 -= op2}. {@code op1} must have enough place to store
   * the result (i.e. {@code op1.bitLength() >= op2.bitLength()}). Both should
   * be positive (what implies that {@code op1 >= op2}).
   *
   * @param op1 the input minuend, and the output result.
   * @param op2 the subtrahend
   */
  def inplaceSubtract(op1: BigInteger, op2: BigInteger) {
    subtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /**
   * @see BigInteger#subtract(BigInteger) .
   * @param op1
   * @param op2
   * @return
   */
  def subtract(op1: BigInteger, op2: BigInteger): BigInteger = {
    var resSign: Int = 0
    var resDigits: Array[Int] = null
    val op1Sign = op1.sign
    val op2Sign = op2.sign
    if (op2Sign == 0) {
      return op1
    }
    if (op1Sign == 0) {
      return op2.negate()
    }
    val op1Len = op1.numberLength
    val op2Len = op2.numberLength
    if (op1Len + op2Len == 2) {
      var a = (op1.digits(0) & 0xFFFFFFFFL)
      var b = (op2.digits(0) & 0xFFFFFFFFL)
      if (op1Sign < 0) {
        a = -a
      }
      if (op2Sign < 0) {
        b = -b
      }
      return BigInteger.valueOf(a - b)
    }
    val cmp = (if ((op1Len != op2Len)) (if ((op1Len > op2Len)) 1 else -1) else Elementary.compareArrays(op1.digits,
      op2.digits, op1Len))
    if (cmp == BigInteger.LESS) {
      resSign = -op2Sign
      resDigits = if ((op1Sign == op2Sign)) subtract(op2.digits, op2Len, op1.digits, op1Len) else add(op2.digits,
        op2Len, op1.digits, op1Len)
    } else {
      resSign = op1Sign
      if (op1Sign == op2Sign) {
        if (cmp == BigInteger.EQUALS) {
          return BigInteger.ZERO
        }
        resDigits = subtract(op1.digits, op1Len, op2.digits, op2Len)
      } else {
        resDigits = add(op1.digits, op1Len, op2.digits, op2Len)
      }
    }
    val res = new BigInteger(resSign, resDigits.length, resDigits)
    res.cutOffLeadingZeroes()
    res
  }


  /**
   * Addss the value represented by {@code b} to the value represented by
   * {@code a}. It is assumed the magnitude of a is not less than the magnitude
   * of b.
   *
   * @return {@code a + b}
   */
  private def add(a: Array[Int],
                  aSize: Int,
                  b: Array[Int],
                  bSize: Int): Array[Int] = {
    val res = new Array[Int](aSize + 1)
    add(res, a, aSize, b, bSize)
    res
  }

  /**
   * Performs {@code res = a + b}.
   */
  private def add(res: Array[Int],
                  a: Array[Int],
                  aSize: Int,
                  b: Array[Int],
                  bSize: Int): Unit = {
    var i: Int = 0
    var carry: Long = (a(0) & 0xFFFFFFFFL) + (b(0) & 0xFFFFFFFFL)
    res(0) = carry.toInt
    carry >>= 32
    if (aSize >= bSize) {
      i = 1
      while (i < bSize) {
        carry += (a(i) & 0xFFFFFFFFL) + (b(i) & 0xFFFFFFFFL)
        res(i) = carry.toInt
        carry >>= 32
        i += 1
      }
      while (i < aSize) {
        carry += a(i) & 0xFFFFFFFFL
        res(i) = carry.toInt
        carry >>= 32
        i += 1
      }
    } else {
      i = 1
      while (i < aSize) {
        carry += (a(i) & 0xFFFFFFFFL) + (b(i) & 0xFFFFFFFFL)
        res(i) = carry.toInt
        carry >>= 32
        i += 1
      }
      while (i < bSize) {
        carry += b(i) & 0xFFFFFFFFL
        res(i) = carry.toInt
        carry >>= 32
        i += 1
      }
    }
    if (carry != 0) {
      res(i) = carry.toInt
    }
  }

  /**
   * Performs {@code res = b - a}.
   */
  private def inverseSubtract(res: Array[Int],
                              a: Array[Int],
                              aSize: Int,
                              b: Array[Int],
                              bSize: Int) {
    var i: Int = 0
    var borrow:Long = 0
    if (aSize < bSize) {
      i = 0
      while (i < aSize) {
        borrow += (b(i) & 0xFFFFFFFFL) - (a(i) & 0xFFFFFFFFL)
        res(i) = borrow.toInt
        borrow >>= 32 // -1 or 0
        i += 1
      }
      while (i < bSize) {
        borrow += b(i) & 0xFFFFFFFFL
        res(i) = borrow.toInt
        borrow >>= 32 // -1 or 0
        i += 1
      }
    } else {
      i = 0
      while (i < bSize) {
        borrow += (b(i) & 0xFFFFFFFFL) - (a(i) & 0xFFFFFFFFL)
        res(i) = borrow.toInt
        borrow >>= 32 // -1 or 0
        i += 1
      }
      while (i < aSize) {
        borrow -= a(i) & 0xFFFFFFFFL
        res(i) = borrow.toInt
        borrow >>= 32 // -1 or 0
        i += 1
      }
    }
  }

  /**
   * Subtracts the value represented by {@code b} from the value represented by
   * {@code a}. It is assumed the magnitude of a is not less than the magnitude
   * of b.
   *
   * @return {@code a - b}
   */
  private def subtract(a: Array[Int],
                       aSize: Int,
                       b: Array[Int],
                       bSize: Int): Array[Int] = {
    val res = new Array[Int](aSize)
    subtract(res, a, aSize, b, bSize)
    res
  }

  /**
   * Performs {@code res = a - b}. It is assumed the magnitude of a is not less
   * than the magnitude of b.
   */
  private def subtract(res: Array[Int],
                       a: Array[Int],
                       aSize: Int,
                       b: Array[Int],
                       bSize: Int) {
    var i: Int = 0
    var borrow:Long = 0
    i = 0
    while (i < bSize) {
      borrow += (a(i) & 0xFFFFFFFFL) - (b(i) & 0xFFFFFFFFL)
      res(i) = borrow.toInt
      borrow >>= 32
      i += 1
    }
    while (i < aSize) {
      borrow += a(i) & 0xFFFFFFFFL
      res(i) = borrow.toInt
      borrow >>= 32
      i += 1
    }
  }

  /**
   * Compares two arrays, representing unsigned integer in little-endian order.
   * Returns +1,0,-1 if a is - respective - greater, equal or lesser then b
   */
  private def unsignedArraysCompare(a: Array[Int],
                                    b: Array[Int],
                                    aSize: Int,
                                    bSize: Int): Int = {
    if (aSize > bSize) {
      1
    } else if (aSize < bSize) {
      -1
    } else {
      var i: Int = 0
      i = aSize - 1
      while (i >= 0 && a(i) == b(i)) {
        i -= 1
      }
      if (i < 0) BigInteger.EQUALS else (if ((a(i) & 0xFFFFFFFFL) < (b(i) & 0xFFFFFFFFL)) BigInteger.LESS else BigInteger.GREATER)
    }
  }
}

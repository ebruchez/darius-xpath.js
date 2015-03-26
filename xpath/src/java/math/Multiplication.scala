/*
 *  Ported by Alistair Johnson from  https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/Multiplication.java
 */

package java.math

/**
 * Object that provides all multiplication of {@link BigInteger} methods.
 */
object Multiplication {

  /**
   * An array with powers of ten that fit in the type {@code int}.
   * ({@code 10^0,10^1,...,10^9})
   */
  val tenPows = Array[Int](
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)


  /**
   * An array with powers of five that fit in the type {@code int}.
   * ({@code 5^0,5^1,...,5^13})
   */
  val fivePows = Array[Int](
    1, 5, 25, 125, 625, 3125, 15625, 78125, 390625,
    1953125, 9765625, 48828125, 244140625, 1220703125)

  /**
   * An array with the first powers of ten in {@code BigInteger} version.
   * ({@code 10^0,10^1,...,10^31})
   */
  val bigTenPows = new Array[BigInteger](32)

  /**
   * An array with the first powers of five in {@code BigInteger} version.
   * ({@code 5^0,5^1,...,5^31})
   */
  val bigFivePows = new Array[BigInteger](32)

  val whenUseKaratsuba = 63

  var fivePow = 1L
  var i: Int = 0
  while (i <= 18) {
    bigFivePows(i) = BigInteger.valueOf(fivePow)
    bigTenPows(i) = BigInteger.valueOf(fivePow << i)
    fivePow *= 5
    i += 1
  }

  while (i < bigTenPows.length) {
    bigFivePows(i) = bigFivePows(i - 1).multiply(bigFivePows(1))
    bigTenPows(i) = bigTenPows(i - 1).multiply(BigInteger.TEN)
    i += 1
  }


  /**
   * Multiplies an array of integers by an integer value.
   *
   * @param a the array of integers
   * @param aSize the number of elements of intArray to be multiplied
   * @param factor the multiplier
   * @return the top digit of production
   */
  def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int = multiplyByInt(a, a, aSize, factor)

  /**
   * Multiplies a number by a positive integer.
   * @param bi an arbitrary {@code BigInteger}
   * @param factor a positive {@code int} number
   * @return {@code val * factor}
   */
  def multiplyByPositiveInt(bi: BigInteger, factor: Int): BigInteger = {
    val resSign: Int = bi.sign
    if (resSign == 0) {
      return BigInteger.ZERO
    }
    val aNumberLength = bi.numberLength
    val aDigits = bi.digits
    if (aNumberLength == 1) {
      val res: Long = unsignedMultAddAdd(aDigits(0), factor, 0, 0)
      val resLo = res.toInt
      val resHi = (res >>> 32).toInt
      return (if ((resHi == 0)) new BigInteger(resSign, resLo) else new BigInteger(resSign, 2, Array(resLo, resHi)))
    }
    val resLength = aNumberLength + 1
    val resDigits = Array.ofDim[Int](resLength)
    resDigits(aNumberLength) = multiplyByInt(resDigits, aDigits, aNumberLength, factor)
    val result = new BigInteger(resSign, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }


  /**
   * Multiplies a number by a power of ten.
   * This method is used in {@code BigDecimal} class.
   * @param bi the number to be multiplied
   * @param exp a positive {@code long} exponent
   * @return {@code val * 10<sup>exp</sup>}
   */
  def multiplyByTenPow(bi: BigInteger, exp: Long): BigInteger = {
    if (exp < tenPows.length) multiplyByPositiveInt(bi, tenPows(exp.toInt))
    else bi.multiply(powerOf10(exp))
  }

  /**
   * Performs a<sup>2</sup>.
   *
   * @param a The number to square.
   * @param aLen The length of the number to square.
   */
  def square(a: Array[Int], aLen: Int, res: Array[Int]): Array[Int]  = {
   // val res: Array[Int] =  Array.ofDim[Int]( resLen)
    var carry: Long = 0l

    for (i <- 0 until aLen) {
      carry = 0
      for (j <- i + 1 until aLen) {
        carry = unsignedMultAddAdd(a(i), a(j), res(i + j), carry.toInt)
        res(i + j) = carry.toInt
        carry >>>= 32
      }
      res(i + aLen) = carry.toInt
    }
    BitLevel.shiftLeftOneBit(res, res, aLen << 1)
    carry = 0
    var i = 0
    var index = 0
    while (i < aLen) {
      carry = unsignedMultAddAdd(a(i), a(i), res(index), carry.toInt)
      res(index) = carry.toInt
      carry >>>= 32
      index += 1
      carry += res(index) & 0xFFFFFFFFL
      res(index) = carry.toInt
      carry >>>= 32
      i += 1
      index += 1
    }
    res
  }

  /**
   * Computes the value unsigned ((uint)a*(uint)b + (uint)c + (uint)d). This
   * method could improve the readability and performance of the code.
   *
   * @param a parameter 1
   * @param b parameter 2
   * @param c parameter 3
   * @param d parameter 4
   * @return value of expression
   */
  def unsignedMultAddAdd(a: Int,
                         b: Int,
                         c: Int,
                         d: Int): Long = {
    (a & 0xFFFFFFFFL) * (b & 0xFFFFFFFFL) + (c & 0xFFFFFFFFL) +
      (d & 0xFFFFFFFFL)
  }

  /**
   * Performs the multiplication with the Karatsuba's algorithm. <b>Karatsuba's
   * algorithm:</b> <tt>
   *             u = u<sub>1</sub> * B + u<sub>0</sub><br>
   *             v = v<sub>1</sub> * B + v<sub>0</sub><br>
   *
   *
   *  u*v = (u<sub>1</sub> * v<sub>1</sub>) * B<sub>2</sub> + ((u<sub>1</sub> - u<sub>0</sub>) * (v<sub>0</sub> - v<sub>1</sub>) + u<sub>1</sub> * v<sub>1</sub> +
   *  u<sub>0</sub> * v<sub>0</sub> ) * B + u<sub>0</sub> * v<sub>0</sub><br>
   *</tt>
   *
   * @param op1 first factor of the product
   * @param op2 second factor of the product
   * @return {@code op1 * op2}
   * @see #multiply(BigInteger, BigInteger)
   */
  def karatsuba(val1: BigInteger, val2: BigInteger): BigInteger = {
    var op1 = val1
    var op2 = val2

    if (op2.numberLength > op1.numberLength) {
      val temp: BigInteger = op1
      op1 = op2
      op2 = temp
    }
    if (op2.numberLength < whenUseKaratsuba) {
      return multiplyPAP(op1, op2)
    }
    /*
     * Karatsuba: u = u1*B + u0 v = v1*B + v0 u*v = (u1*v1)*B^2 +
     * ((u1-u0)*(v0-v1) + u1*v1 + u0*v0)*B + u0*v0
     */
    val ndiv2 = (op1.numberLength & 0xFFFFFFFE) << 4
    val upperOp1 = op1.shiftRight(ndiv2)
    val upperOp2 = op2.shiftRight(ndiv2)
    val lowerOp1 = op1.subtract(upperOp1.shiftLeft(ndiv2))
    val lowerOp2 = op2.subtract(upperOp2.shiftLeft(ndiv2))

    var upper = karatsuba(upperOp1, upperOp2)
    val lower = karatsuba(lowerOp1, lowerOp2)
    var middle = karatsuba(upperOp1.subtract(lowerOp1), lowerOp2.subtract(upperOp2))
    middle = middle.add(upper).add(lower)
    middle = middle.shiftLeft(ndiv2)
    upper = upper.shiftLeft(ndiv2 << 1)
    upper.add(middle).add(lower)
  }

  def multArraysPAP(aDigits: Array[Int],
                    aLen: Int,
                    bDigits: Array[Int],
                    bLen: Int,
                    resDigits: Array[Int]): Unit = {

    if (aLen == 0 || bLen == 0) {
      return
    }
    if (aLen == 1) {
      resDigits(bLen) = multiplyByInt(resDigits, bDigits, bLen, aDigits(0))
    } else if (bLen == 1) {
      resDigits(aLen) = multiplyByInt(resDigits, aDigits, aLen, bDigits(0))
    } else {
       multPAP(aDigits, bDigits, resDigits, aLen, bLen)
    }
  }


  def multiply(x: BigInteger, y: BigInteger): BigInteger = karatsuba(x, y)

  /**
   * Multiplies two BigIntegers. Implements traditional scholar algorithm
   * described by Knuth.
   *
   * <br>
   * <tt>
   *         <table border="0">
   * <tbody>
   *
   *
   * <tr>
   * <td align="center">A=</td>
   * <td>a<sub>3</sub></td>
   * <td>a<sub>2</sub></td>
   * <td>a<sub>1</sub></td>
   * <td>a<sub>0</sub></td>
   * <td></td>
   * <td></td>
   * </tr>
   *
   * <tr>
   * <td align="center">B=</td>
   * <td></td>
   * <td>b<sub>2</sub></td>
   * <td>b<sub>1</sub></td>
   * <td>b<sub>1</sub></td>
   * <td></td>
   * <td></td>
   * </tr>
   *
   * <tr>
   * <td></td>
   * <td></td>
   * <td></td>
   * <td>b<sub>0</sub>*a<sub>3</sub></td>
   * <td>b<sub>0</sub>*a<sub>2</sub></td>
   * <td>b<sub>0</sub>*a<sub>1</sub></td>
   * <td>b<sub>0</sub>*a<sub>0</sub></td>
   * </tr>
   *
   * <tr>
   * <td></td>
   * <td></td>
   * <td>b<sub>1</sub>*a<sub>3</sub></td>
   * <td>b<sub>1</sub>*a<sub>2</sub></td>
   * <td>b<sub>1</sub>*a1</td>
   * <td>b<sub>1</sub>*a0</td>
   * </tr>
   *
   * <tr>
   * <td>+</td>
   * <td>b<sub>2</sub>*a<sub>3</sub></td>
   * <td>b<sub>2</sub>*a<sub>2</sub></td>
   * <td>b<sub>2</sub>*a<sub>1</sub></td>
   * <td>b<sub>2</sub>*a<sub>0</sub></td>
   * </tr>
   *
   * <tr>
   * <td></td>
   * <td>______</td>
   * <td>______</td>
   * <td>______</td>
   * <td>______</td>
   * <td>______</td>
   * <td>______</td>
   * </tr>
   *
   * <tr>
   *
   * <td align="center">A*B=R=</td>
   * <td align="center">r<sub>5</sub></td>
   * <td align="center">r<sub>4</sub></td>
   * <td align="center">r<sub>3</sub></td>
   * <td align="center">r<sub>2</sub></td>
   * <td align="center">r<sub>1</sub></td>
   * <td align="center">r<sub>0</sub></td>
   * <td></td>
   * </tr>
   *
   * </tbody>
   * </table>
   *
   *</tt>
   *
   * @param op1 first factor of the multiplication {@code op1 >= 0}
   * @param op2 second factor of the multiplication {@code op2 >= 0}
   * @return a {@code BigInteger} of value {@code op1 * op2}
   */
  def multiplyPAP(a: BigInteger, b: BigInteger): BigInteger = {
    val aLen = a.numberLength
    val bLen = b.numberLength
    val resLength = aLen + bLen
    val resSign = if ((a.sign != b.sign)) -1 else 1
    if (resLength == 2) {
      val v = unsignedMultAddAdd(a.digits(0), b.digits(0), 0, 0)
      val valueLo = v.toInt
      val valueHi = (v >>> 32).toInt
      return (if ((valueHi == 0)) new BigInteger(resSign, valueLo) else new BigInteger(resSign, 2, Array(valueLo, valueHi)))
    }
    val aDigits = a.digits
    val bDigits = b.digits
    val resDigits = Array.ofDim[Int](resLength)
    multArraysPAP(aDigits, aLen, bDigits, bLen, resDigits)
    val result = new BigInteger(resSign, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }



  def pow(base: BigInteger, exponent: Int): BigInteger = {
    var _exponent = exponent
    var res = BigInteger.ONE
    var acc = base
    while (_exponent > 1) {
      if ((_exponent & 1) != 0) {
        res = res.multiply(acc)
      }
      acc = if (acc.numberLength == 1) acc.multiply(acc) else new BigInteger(1, square(acc.digits, acc.numberLength,
        Array.ofDim[Int](acc.numberLength << 1)))
      _exponent >>= 1
    }
    res = res.multiply(acc)
    res
  }
  
  /**
   * It calculates a power of ten, which exponent could be out of 32-bit range.
   * Note that internally this method will be used in the worst case with
   * an exponent equals to: {@code Integer.MAX_VALUE - Integer.MIN_VALUE}.
   * @param exp the exponent of power of ten, it must be positive.
   * @return a {@code BigInteger} with value {@code 10<sup>exp</sup>}.
   */
  def powerOf10(exp: Long): BigInteger = {
    // "SMALL POWERS"
    if (exp < bigTenPows.length) bigTenPows(exp.toInt)
    else if (exp <= 50) BigInteger.TEN.pow(exp.toInt)
    // "LARGE POWERS"
    else if (exp <= java.lang.Integer.MAX_VALUE) bigFivePows(1).pow(exp.toInt).shiftLeft(exp.toInt)
    else { //"HUGE POWERS"
      val powerOfFive = bigFivePows(1).pow(Integer.MAX_VALUE)
      var res: BigInteger  = powerOfFive
      var longExp = exp -  java.lang.Integer.MAX_VALUE
      val intExp = (exp % java.lang.Integer.MAX_VALUE).toInt
      while (longExp > java.lang.Integer.MAX_VALUE) {
        res = res.multiply(powerOfFive)
        longExp -= java.lang.Integer.MAX_VALUE
      }
      res = res.multiply(bigFivePows(1).pow(intExp))
      res = res.shiftLeft(java.lang.Integer.MAX_VALUE)
      longExp = exp - java.lang.Integer.MAX_VALUE
      while (longExp > java.lang.Integer.MAX_VALUE) {
        res = res.shiftLeft(java.lang.Integer.MAX_VALUE)
        longExp -= java.lang.Integer.MAX_VALUE
      }
      res.shiftLeft(intExp)
    }
  }

  /**
   * Multiplies a number by a power of five.
   * This method is used in {@code BigDecimal} class.
   * @param val the number to be multiplied
   * @param exp a positive {@code int} exponent
   * @return {@code val * 5<sup>exp</sup>}
   */
  def multiplyByFivePow(bi: BigInteger, exp: Int): BigInteger = {
    if (exp < fivePows.length) multiplyByPositiveInt(bi, fivePows(exp))
    else if (exp < bigFivePows.length) bi.multiply(bigFivePows(exp))
    else bi.multiply(bigFivePows(1).pow(exp))
  }

  private def multiplyByInt(res: Array[Int],
                            a: Array[Int],
                            aSize: Int,
                            factor: Int): Int = {
    var carry:Long = 0
    for (i <- 0 until aSize) {
      carry = unsignedMultAddAdd(a(i), factor, carry.toInt, 0)
      res(i) = carry.toInt
      carry >>>= 32
    }
    carry.toInt
  }

  private def multPAP(a: Array[Int],
              b: Array[Int],
              t: Array[Int],
              aLen: Int,
              bLen: Int): Unit = {
    //val resDigits = Array.ofDim[Int](resLen)
    if (a == b && aLen == bLen) {
      square(a, aLen, t)
      return
    }

    for (i <- 0 until aLen) {
      var carry: Long = 0
      val aI = a(i)
      for (j <- 0 until bLen) {
        carry = unsignedMultAddAdd(aI, b(j), t(i + j), carry.toInt)
        t(i + j) = carry.toInt
        carry >>>= 32
      }
      t(i + bLen) = carry.toInt
    }
  }
}



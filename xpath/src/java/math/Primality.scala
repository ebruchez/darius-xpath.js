/*
 *  Ported by Alistair Johnson from  https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/math/Primality.java
 */

package java.math

import java.util.Arrays
import java.util.Random

/**
 * Provides primality probabilistic methods.
 */
object Primality {
  private val BITS = Array(
    0, 0, 1854, 1233, 927, 747, 627, 543, 480, 431, 393, 361, 335, 314, 295,
    279, 265, 253, 242, 232, 223, 216, 181, 169, 158, 150, 145, 140, 136,
    132, 127, 123, 119, 114, 110, 105, 101, 96, 92, 87, 83, 78, 73, 69, 64,
    59, 54, 49, 44, 38, 32, 26, 1)

  /** All prime numbers with bit length lesser than 10 bits. */
  private val primes = Array[Int]( 2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
    31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
    103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
    173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239,
    241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313,
    317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397,
    401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467,
    479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569,
    571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643,
    647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733,
    739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823,
    827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911,
    919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009,
    1013, 1019, 1021 )


  /**
   * It encodes how many i-bit primes there are in the table for {@code
   * i=2,...,10}. For example {@code offsetPrimes[6]} says that from index
   * {@code 11} exists {@code 7} consecutive {@code 6}-bit prime numbers in the
   * array.
   */
  private val offsetPrimes = Array(
    null, null, Array(0, 2), Array(2, 2), Array(4, 2), Array(6, 5), Array(11, 7),
    Array(18, 13), Array(31, 23), Array(54, 43), Array(97, 75))


  /**
   * All {@code BigInteger} prime numbers with bit length lesser than 8 bits.
   */
  private val BIprimes = new Array[BigInteger](primes.length)

  // To initialize the dual table of BigInteger primes
  for (i <- 0 until primes.length) {
    BIprimes(i) = BigInteger.valueOf(primes(i))
  }

  /**
   * A random number is generated until a probable prime number is found.
   *
   * @see BigInteger#BigInteger(int,int,Random)
   * @see BigInteger#probablePrime(int,Random)
   * @see #isProbablePrime(BigInteger, int)
   */
  def consBigInteger(bitLength: Int, certainty: Int, rnd: Random): BigInteger = {
    // PRE: bitLength >= 2;
    // For small numbers get a random prime from the prime table
    if (bitLength <= 10) {
      val rp = offsetPrimes(bitLength)
      return BIprimes(rp(0) + rnd.nextInt(rp(1)))
    }
    val shiftCount = (-bitLength) & 31
    var last = (bitLength + 31) >> 5
    val n = new BigInteger(1, last, new Array[Int](last))
    last -= 1
    do {
      // To fill the array with random integers
      for (i <- 0 until n.numberLength) {
        n.digits(i) = rnd.nextInt()
      }
      // To fix to the correct bitLength
      n.digits(last) |= 0x80000000
      n.digits(last) >>>= shiftCount
      // To create an odd number
      n.digits(0) |= 1
    } while (!isProbablePrime(n, certainty));
    n
  }

  /**
   * @see BigInteger#isProbablePrime(int)
   * @see #millerRabin(BigInteger, int)
   * @ar.org.fitc.ref Optimizations: "A. Menezes - Handbook of applied
   *                  Cryptography, Chapter 4".
   */
  def isProbablePrime(n: BigInteger, certainty: Int): Boolean = {
    // PRE: n >= 0;
    if ((certainty <= 0) || ((n.numberLength == 1) && (n.digits(0) == 2))) {
      return true
    }
    // To discard all even numbers
    if (!n.testBit(0)) {
      return false
    }
    // To check if 'n' exists in the table (it fit in 10 bits)
    if ((n.numberLength == 1) && ((n.digits(0) & 0XFFFFFC00) == 0)) {
      return (Arrays.binarySearch(primes, n.digits(0)) >= 0)
    }
    // To check if 'n' is divisible by some prime of the table
    for (i <- 1 until primes.length) {
      if (Division.remainderArrayByInt(n.digits, n.numberLength, primes(i)) == 0) {
        return false
      }
    }

    // To set the number of iterations necessary for Miller-Rabin test
    var i: Int = 0
    val bitLength = n.bitLength()
    i = 2
    while (bitLength < BITS(i)) {
      i += 1
    }

    val newCertainty = Math.min(i, 1 + ((certainty - 1) >> 1))
    millerRabin(n, newCertainty)
  }

  /**
   * It uses the sieve of Eratosthenes to discard several composite numbers in
   * some appropriate range (at the moment {@code [this, this + 1024]}). After
   * this process it applies the Miller-Rabin test to the numbers that were not
   * discarded in the sieve.
   *
   * @see BigInteger#nextProbablePrime()
   * @see #millerRabin(BigInteger, int)
   */
  def nextProbablePrime(n: BigInteger): BigInteger = {
    // PRE: n >= 0
    var i: Int = 0
    var j: Int = 0
    var certainty: Int = 0
    val gapSize = 1024  // for searching of the next probable prime number
    val modules = new Array[Int](primes.length)
    val isDivisible = new Array[Boolean](gapSize)

    // If n < "last prime of table" searches next prime in the table
    if ((n.numberLength == 1) && (n.digits(0) >= 0) && (n.digits(0) < primes(primes.length - 1))) {
      i = 0
      while (n.digits(0) >= primes(i)) {
        i += 1
      }
      return BIprimes(i)
    }

    /*
     * Creates a "N" enough big to hold the next probable prime Note that: N <
     * "next prime" < 2*N
     */
    val startPoint: BigInteger =  new BigInteger(1, n.numberLength, new Array[Int](n.numberLength + 1))
    System.arraycopy(n.digits, 0, startPoint.digits, 0, n.numberLength)
    // To fix N to the "next odd number"
    if (n.testBit(0)) {
      val t = startPoint
      Elementary.inplaceAdd(startPoint, 2)
      require(startPoint.longValue > t.longValue)
    } else {
      startPoint.digits(0) |= 1
    }
    // To set the improved certainly of Miller-Rabin
    j = startPoint.bitLength()
    certainty = 2
    while (j < BITS(certainty)) {
      certainty += 1
    }

    // To calculate modules: N mod p1, N mod p2, ... for first primes.
    i = 0
    while (i < primes.length) {
      modules(i) = Division.remainder(startPoint, primes(i)) - gapSize
      i += 1
    }

    val probPrime: BigInteger = startPoint.copy()
    while (true) {
      // At this point, all numbers in the gap are initialized as
      // probably primes
      Arrays.fill(isDivisible, false)
      // To discard multiples of first primes
      i = 0
      while (i < primes.length) {
        modules(i) = (modules(i) + gapSize) % primes(i)
        j = if ((modules(i) == 0)) 0 else (primes(i) - modules(i))
        while (j < gapSize) {
          isDivisible(j) = true
          j += primes(i)
        }
        i += 1
      }
      // To execute Miller-Rabin for non-divisible numbers by all first
      // primes
      j = 0
      while (j < gapSize) {
        if (!isDivisible(j)) {
          Elementary.inplaceAdd(probPrime, j)
          if (millerRabin(probPrime, certainty)) {
            return probPrime
          }
        }
        j += 1
      }
      Elementary.inplaceAdd(startPoint, gapSize)
    }
    // Is this unreachable?
    probPrime
  }

  /**
   * The Miller-Rabin primality test.
   *
   * @param n the input number to be tested.
   * @param t the number of trials.
   * @return {@code false} if the number is definitely compose, otherwise
   *         {@code true} with probability {@code 1 - 4<sup>(-t)</sup>}.
   * @ar.org.fitc.ref "D. Knuth, The Art of Computer Programming Vo.2, Section
   *                  4.5.4., Algorithm P"
   */
  private def millerRabin(n: BigInteger, t: Int): Boolean = {
    // PRE: n >= 0, t >= 0
    var x: BigInteger = null
    var y: BigInteger = null
    val nMinus1 = n.subtract(BigInteger.ONE)
    val bitLength = nMinus1.bitLength()
    val k = nMinus1.getLowestSetBit
    val q = nMinus1.shiftRight(k)
    val rnd = new Random()
    for (i <- 0 until t) {
      // To generate a witness 'x', first it use the primes of table
      if (i < primes.length) {
        x = BIprimes(i)
      } else {
        /*
         * It generates random witness only if it's necesssary. Note that all
         * methods would call Miller-Rabin with t <= 50 so this part is only to
         * do more robust the algorithm
         */
        do {
          x = new BigInteger(bitLength, rnd)
        } while ((x.compareTo(n) >= BigInteger.EQUALS) || (x.sign == 0) ||
          x.isOne)
      }
      y = x.modPow(q, n)

      if (y.isOne || y == nMinus1) {
        //continue changed to if/else
      }else {
        for (j <- 1 until k) {
          if (y == nMinus1) {
            //continue changed to if/else
          } else {
            y = y.multiply(y).mod(n)
            if (y.isOne) {
              return false
            }
          }
        }
        if (y != nMinus1) {
          return false
        }
      }
    }
    true
  }
}

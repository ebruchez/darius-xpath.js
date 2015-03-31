/*
*  Ported by Alistair Johnson from  https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/MathContext.java
*
*  regionMatches ported from https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/String.java
*/

package java.math

import java.io.ObjectInputStream
import java.io.Serializable

import InternalPreconditions._

object MathContext {

  /**
   * A {@code MathContext} which corresponds to the <a href="http://en.wikipedia.org/wiki/IEEE_754-1985">IEEE 754</a> quadruple
   * decimal precision format: 34 digit precision and
   * {@link RoundingMode#HALF_EVEN} rounding.
   */
  val DECIMAL128 = MathContext(34, RoundingMode.HALF_EVEN)

  /**
   * A {@code MathContext} which corresponds to the <a href="http://en.wikipedia.org/wiki/IEEE_754-1985">IEEE 754</a> single decimal
   * precision format: 7 digit precision and {@link RoundingMode#HALF_EVEN}
   * rounding.
   */
  val DECIMAL32 = MathContext(7, RoundingMode.HALF_EVEN)

  /**
   * A {@code MathContext} which corresponds to the <a href="http://en.wikipedia.org/wiki/IEEE_754-1985">IEEE 754</a> double decimal
   * precision format: 16 digit precision and {@link RoundingMode#HALF_EVEN}
   * rounding.
   */
  val DECIMAL64 = MathContext(16, RoundingMode.HALF_EVEN)

  /**
   * A {@code MathContext} for unlimited precision with
   * {@link RoundingMode#HALF_UP} rounding.
   */
  val UNLIMITED = MathContext(0, RoundingMode.HALF_UP)

  private def getArgs(s: String): (Int, RoundingMode) = {
    checkNotNull(s, "null string")
    val precisionLength:Int    = "precision=".length
    val roundingModeLength:Int = "roundingMode=".length
    val spaceIndex: Int = s.indexOf(' ', precisionLength)
    if (!s.startsWith("precision=") || spaceIndex == -1) {
      throw invalidMathContext("Missing precision", s)
    }
    val precisionString = s.substring(precisionLength, spaceIndex)

    val precision = try {
      java.lang.Integer.parseInt(precisionString)
    }
    catch {
      case _: Throwable => throw invalidMathContext("Bad precision", s)
    }

    var roundingModeStart: Int = spaceIndex + 1
    if (!s.regionMatches(roundingModeStart, "roundingMode=", 0, roundingModeLength)) {
      throw invalidMathContext("Missing rounding mode", s)
    }
    roundingModeStart += roundingModeLength
    val roundingMode: RoundingMode = RoundingMode.valueOf(s.substring(roundingModeStart))

    (precision, roundingMode)
  }


  private def invalidMathContext(reason: String, s: String): IllegalArgumentException = {
    throw new IllegalArgumentException(reason + ": " + s)
  }

}
/**
 *  Immutable objects describing settings such as rounding mode and digit
 *  precision for the numerical operations provided by class {@link BigDecimal}.
 *
 *  Constructs a new {@code MathContext} with the specified precision and
 *  with the specified rounding mode. If the precision passed is zero, then
 *  this implies that the computations have to be performed exact, the
 *  rounding mode in this case is irrelevant.
 *
 *  @param precision
 *            the precision for the new {@code MathContext}.
 *  @param roundingMode
 *            the rounding mode for the new {@code MathContext}.
 */
case class MathContext(precision: Int, roundingMode: RoundingMode) extends Serializable {

  checkValid()

  def getPrecision = precision
  def getRoundingMode = roundingMode

  /**
   * Constructs a new {@code MathContext} with the specified precision and
   * with the rounding mode {@link RoundingMode#HALF_UP HALF_UP}. If the
   * precision passed is zero, then this implies that the computations have to
   * be performed exact, the rounding mode in this case is irrelevant.
   *
   * @param precision
   *            the precision for the new {@code MathContext}.
   */
  def this(precision: Int) {
    this(precision, RoundingMode.HALF_UP)
  }

  private def this(args:(Int, RoundingMode)) {
    this(args._1, args._2)
  }

  /**
   * Constructs a new {@code MathContext} from a string. The string has to
   * specify the precision and the rounding mode to be used and has to follow
   * the following syntax: "precision=&lt;precision&gt; roundingMode=&lt;roundingMode&gt;"
   * This is the same form as the one returned by the {@link #toString}
   * method.
   *
   * @throws IllegalArgumentException
     *             if the string is not in the correct format or if the
   *             precision specified is < 0.
   */
  def this(s: String) {
    this(MathContext.getArgs(s))
    checkValid()
  }


  private def checkValid(): Unit = {
    if (precision < 0) {
      throw new IllegalArgumentException("Negative precision: " + precision)
    }
    if (roundingMode == null) {
      throw new NullPointerException("roundingMode == null")
    }
  }

  /**
   * Returns true if x is a {@code MathContext} with the same precision
   * setting and the same rounding mode as this {@code MathContext} instance.
   *
   * @param x
     *            object to be compared.
   * @return {@code true} if this {@code MathContext} instance is equal to the
   *         {@code x} argument; {@code false} otherwise.
   */
  override def equals(x: Any): Boolean = {
    (x.isInstanceOf[MathContext]) &&
      (x.asInstanceOf[MathContext].precision == precision) &&
      (x.asInstanceOf[MathContext].roundingMode == roundingMode)
  }

  /**
   * Returns the hash code for this {@code MathContext} instance.
   *
   * @return the hash code for this {@code MathContext}.
   */
  override def hashCode(): Int = {
    (precision << 3) | roundingMode.ordinal()
  }

  /**
   * Returns the string representation for this {@code MathContext} instance.
   * The string has the form
   * {@code
   * "precision=<precision> roundingMode=<roundingMode>"
   * } where {@code <precision>} is an integer describing the number
   * of digits used for operations and {@code <roundingMode>} is the
   * string representation of the rounding mode.
   *
   * @return a string representation for this {@code MathContext} instance
   */
  override def toString(): String = {
    "precision=" + precision + " roundingMode=" + roundingMode
  }

  /**
   * Makes checks upon deserialization of a {@code MathContext} instance.
   * Checks whether {@code precision >= 0} and {@code roundingMode != null}
   *
   * @throws StreamCorruptedException
     *             if {@code precision < 0}
   * @throws StreamCorruptedException
     *             if {@code roundingMode == null}
   */
  private def readObject(s: ObjectInputStream): Unit = {
    s.defaultReadObject()
    checkValid()
  }
}

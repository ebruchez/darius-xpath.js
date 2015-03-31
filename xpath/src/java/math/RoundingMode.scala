/*
 *  Ported by Alistair Johnson from  https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/RoundingMode.java
 */
package java.math

abstract class RoundingMode private (_index: Int,
                                     _name: String) extends java.io.Serializable {
  def name(): String = _name
  def ordinal(): Int = _index

  override def toString() = name()
}

/**
 * Specifies the rounding behavior for operations whose results cannot be
 * represented exactly.
 */
object RoundingMode   {

  /**
   * Rounding mode where positive values are rounded towards positive infinity
   * and negative values towards negative infinity.
   * <br>
   * Rule: {@code x.round().abs() >= x.abs()}
   */

  final val UP: RoundingMode = new RoundingMode(BigDecimal.ROUND_UP, "UP"){}

  /**
   * Rounding mode where the values are rounded towards zero.
   * <br>
   * Rule: {@code x.round().abs() <= x.abs()}
   */
  final val DOWN: RoundingMode = new RoundingMode(BigDecimal.ROUND_DOWN, "DOWN"){}

  /**
   * Rounding mode to round towards positive infinity. For positive values
   * this rounding mode behaves as {@link #UP}, for negative values as
   * {@link #DOWN}.
   * <br>
   * Rule: {@code x.round() >= x}
   */
  final val CEILING: RoundingMode = new RoundingMode(BigDecimal.ROUND_CEILING, "CEILING"){}

  /**
   * Rounding mode to round towards negative infinity. For positive values
   * this rounding mode behaves as {@link #DOWN}, for negative values as
   * {@link #UP}.
   * <br>
   * Rule: {@code x.round() <= x}
   */
  final val FLOOR: RoundingMode = new RoundingMode(BigDecimal.ROUND_FLOOR, "FLOOR"){}

  /**
   * Rounding mode where values are rounded towards the nearest neighbor. Ties
   * are broken by rounding up.
   */
  final val HALF_UP: RoundingMode = new RoundingMode(BigDecimal.ROUND_HALF_UP, "HALF_UP"){}

  /**
   * Rounding mode where values are rounded towards the nearest neighbor. Ties
   * are broken by rounding down.
   */
  final val HALF_DOWN: RoundingMode = new RoundingMode(BigDecimal.ROUND_HALF_DOWN, "HALF_DOWN"){}

  /**
   * Rounding mode where values are rounded towards the nearest neighbor. Ties
   * are broken by rounding to the even neighbor.
   */
  final val HALF_EVEN: RoundingMode = new RoundingMode(BigDecimal.ROUND_HALF_EVEN, "HALF_EVEN"){}

  /**
   * Rounding mode where the rounding operations throws an ArithmeticException
   * for the case that rounding is necessary, i.e. for the case that the value
   * cannot be represented exactly.
   */
  final val UNNECESSARY: RoundingMode = new RoundingMode(BigDecimal.ROUND_UNNECESSARY, "UNNECESSARY"){}

  private[this] val _values: Array[RoundingMode] =
    Array(UP, DOWN, CEILING, FLOOR, HALF_UP, HALF_DOWN, HALF_EVEN, UNNECESSARY)

  def values(): Array[RoundingMode] = _values.clone()

  def valueOf(name: String): RoundingMode = {
    _values.find(_.name == name).getOrElse(
      throw new IllegalArgumentException("No enum const TimeUnit." + name))
  }

  /**
   * Converts rounding mode constants from class {@code BigDecimal} into
   * {@code RoundingMode} values.
   *
   * @param mode
     *            rounding mode constant as defined in class {@code BigDecimal}
   * @return corresponding rounding mode object
   */
  def valueOf(mode: Int): RoundingMode = mode match {
    case BigDecimal.ROUND_CEILING ⇒ CEILING
    case BigDecimal.ROUND_DOWN ⇒ DOWN
    case BigDecimal.ROUND_FLOOR ⇒ FLOOR
    case BigDecimal.ROUND_HALF_DOWN ⇒ HALF_DOWN
    case BigDecimal.ROUND_HALF_EVEN ⇒ HALF_EVEN
    case BigDecimal.ROUND_HALF_UP ⇒ HALF_UP
    case BigDecimal.ROUND_UNNECESSARY ⇒ UNNECESSARY
    case BigDecimal.ROUND_UP ⇒ UP
    case _ ⇒ throw new IllegalArgumentException("Invalid rounding mode")
  }
}


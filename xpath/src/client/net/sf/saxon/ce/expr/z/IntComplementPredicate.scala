package client.net.sf.saxon.ce.expr.z

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An IntPredicate formed as the complement of another predicate;
 * it matches an integer if the operand does not, and vice versa.
 */
class IntComplementPredicate(var p1: IntPredicate) extends IntPredicate {

  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  def matches(value: Int): Boolean = !p1.matches(value)

  /**
   * Get the operand
   * @return the negated predicate
   */
  def getOperand(): IntPredicate = p1
}

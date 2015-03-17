package client.net.sf.saxon.ce.expr.z

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface defining a predicate that can be tested to determine whether an integer
 * satisfies, or does not satisfy, some condition.
 */
trait IntPredicate {

  /**
   * Ask whether a given value matches this predicate
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  def matches(value: Int): Boolean
}

package client.net.sf.saxon.ce.expr.z

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An implementation of IntPredicate that tests whether a given integer is a member
 * of some IntSet
 */
class IntSetPredicate(var set: IntSet) extends IntPredicate {

  if (set == null) {
    throw new NullPointerException()
  }

  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  def matches(value: Int): Boolean = set.contains(value)

  /**
   * Get the underlying IntSet
   * @return the underlying IntSet
   */
  def getIntSet(): IntSet = set
}

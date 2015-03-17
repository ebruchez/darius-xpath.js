package client.net.sf.saxon.ce.expr.z

import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An IntPredicate that matches a single specific integer
 */
class IntValuePredicate(@BeanProperty var target: Int) extends IntPredicate {

  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  def matches(value: Int): Boolean = value == target
}

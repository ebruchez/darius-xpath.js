package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.value.AtomicValue
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Comparer used for comparing descending keys. This simply returns the inverse of the result
 * delivered by the base comparer.
 */
class DescendingComparer(@BeanProperty var baseComparer: AtomicComparer) extends AtomicComparer {

  def getCollator(): StringCollator = baseComparer.getCollator

  /**
   * Compare two objects.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are of the wrong type for this Comparer
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    0 - baseComparer.compareAtomicValues(a, b)
  }

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   *
   * @param a the first object to be compared. It is intended that this should be an instance
   *          of AtomicValue, though this restriction is not enforced. If it is a StringValue, the
   *          collator is used to compare the values, otherwise the value must implement the equals() method.
   * @param b the second object to be compared. This must be comparable with the first object: for
   *          example, if one is a string, they must both be strings.
   * @return true if the values are equal, false if not
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = baseComparer.comparesEqual(a, b)
}

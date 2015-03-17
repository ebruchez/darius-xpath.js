package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.value.AtomicValue
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An AtomicComparer used for comparing strings, untypedAtomic values, and URIs using a collation.
 * A CollatingAtomicComparer is used when it is known in advance that both operands will be of these
 * types. This enables all conversions and promotions to be bypassed: the string values of both operands
 * are simply extracted and passed to the collator for comparison.
 *
 * @author Michael H. Kay
 *
 */
class CollatingAtomicComparer(collator: StringCollator) extends AtomicComparer {

  @BeanProperty
  var collator: StringCollator = if (collator == null) CodepointCollator.getInstance else collator

  /**
   * Compare two AtomicValue objects according to the rules for their data type. UntypedAtomic
   * values are compared as if they were strings; if different semantics are wanted, the conversion
   * must be done by the caller.
   *
   * @param a the first object to be compared. It is intended that this should be an instance
   * of AtomicValue, though this restriction is not enforced. If it is a StringValue, the
   * collator is used to compare the values, otherwise the value must implement the java.util.Comparable
   * interface.
   * @param b the second object to be compared. This must be comparable with the first object: for
   * example, if one is a string, they must both be strings.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not comparable
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) {
        return 0
      } else {
        return -1
      }
    } else if (b == null) {
      return +1
    }
    collator.compareStrings(a.getStringValue, b.getStringValue)
  }

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   * @param a the first object to be compared. It is intended that this should be an instance
   * of AtomicValue, though this restriction is not enforced. If it is a StringValue, the
   * collator is used to compare the values, otherwise the value must implement the equals() method.
   * @param b the second object to be compared. This must be comparable with the first object: for
   * example, if one is a string, they must both be strings.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = compareAtomicValues(a, b) == 0
}

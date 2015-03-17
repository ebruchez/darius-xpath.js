package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.StringValue
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Comparer used for comparing sort keys when data-type="text". The items to be
 * compared are converted to strings, and the strings are then compared using an
 * underlying collator
 *
 * @author Michael H. Kay
 *
 */
class TextComparer(@BeanProperty var baseComparer: AtomicComparer) extends AtomicComparer {

  def getCollator(): StringCollator = baseComparer.getCollator

  /**
   * Compare two Items by converting them to strings and comparing the string values.
   *
   * @param a the first Item to be compared.
   * @param b the second Item to be compared.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not Items, or are items that cannot be convered
   * to strings (e.g. QNames)
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    baseComparer.compareAtomicValues(toStringValue(a), toStringValue(b))
  }

  private def toStringValue(a: AtomicValue): StringValue = {
    if (a.isInstanceOf[StringValue]) {
      a.asInstanceOf[StringValue]
    } else {
      new StringValue((if (a == null) "" else a.getStringValue))
    }
  }

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   *
   * @param a the first object to be compared.
   * @param b the second object to be compared.
   * @return true if the values are equal, false if not
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = compareAtomicValues(a, b) == 0
}

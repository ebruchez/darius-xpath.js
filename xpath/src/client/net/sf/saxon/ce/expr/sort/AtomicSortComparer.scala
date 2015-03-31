// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.`type`.{AtomicType, Type}
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.value.{AtomicValue, StringValue, UntypedAtomicValue}

import scala.beans.BeanProperty

object AtomicSortComparer {

  /**
   * Factory method to get an atomic comparer suitable for sorting or for grouping (operations in which
   * NaN is considered equal to NaN)
   * @param collator Collating comparer to be used when comparing strings. This argument may be null
   * if the itemType excludes the possibility of comparing strings. If the method is called at compile
   * time, this should be a NamedCollation so that it can be cloned at run-time.
   * @param itemType the primitive item type of the values to be compared
   * @param implicitTimezone from the Dynamic context
   * @return a suitable AtomicComparer
   */
  def makeSortComparer(collator: StringCollator, itemType: AtomicType, implicitTimezone: Int): AtomicComparer = {
    if (itemType == AtomicType.STRING || itemType == AtomicType.UNTYPED_ATOMIC || 
      itemType == AtomicType.ANY_URI) {
      if (collator.isInstanceOf[CodepointCollator]) {
        CodepointCollatingComparer.getInstance
      } else {
        new CollatingAtomicComparer(collator)
      }
    } else if (itemType == AtomicType.INTEGER || itemType == AtomicType.DECIMAL || 
      itemType == AtomicType.DOUBLE || 
      itemType == AtomicType.FLOAT || 
      itemType == AtomicType.NUMERIC) {
      ComparableAtomicValueComparer.getInstance
    } else if (itemType == AtomicType.DATE_TIME || itemType == AtomicType.DATE || 
      itemType == AtomicType.TIME) {
      new CalendarValueComparer(implicitTimezone)
    } else {
      new AtomicSortComparer(collator, itemType, implicitTimezone)
    }
  }

  protected var COLLATION_KEY_NaN: StructuredQName = new StructuredQName("saxon", "http://saxon.sf.net/collation-key", 
    "NaN")
}

/**
 * An AtomicComparer used for comparing atomic values of arbitrary item types. It encapsulates
 * a collator that is used when the values to be compared are strings. It also supports
 * a separate method for testing equality of items, which can be used for data types that
 * are not ordered.
 *
 * The AtomicSortComparer is identical to the GenericAtomicComparer except for its handling
 * of NaN: it treats NaN values as lower than any other value, and as equal to each other.
 *
 * @author Michael H. Kay
 *
 */
class AtomicSortComparer protected (@BeanProperty var collator: StringCollator, itemType: AtomicType, var implicitTimezone: Int)
    extends AtomicComparer {

  if (collator == null) {
    this.collator = CodepointCollator.getInstance
  }

  /**
   * Compare two AtomicValue objects according to the rules for their data type. UntypedAtomic
   * values are compared as if they were strings; if different semantics are wanted, the conversion
   * must be done by the caller.
   *
   * @param a the first object to be compared. It is intended that this should normally be an instance
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
    if (a.isInstanceOf[UntypedAtomicValue]) {
      a.asInstanceOf[UntypedAtomicValue].compareTo(b, collator)
    } else if (b.isInstanceOf[UntypedAtomicValue]) {
      -b.asInstanceOf[UntypedAtomicValue].compareTo(a, collator)
    } else if (a.isNaN) {
      if (b.isNaN) 0 else -1
    } else if (b.isNaN) {
      +1
    } else if (a.isInstanceOf[StringValue] && b.isInstanceOf[StringValue]) {
      if (collator.isInstanceOf[CodepointCollator]) {
        collator.asInstanceOf[CodepointCollator].compareCS(a.getStringValue, b.getStringValue)
      } else {
        collator.compareStrings(a.getStringValue, b.getStringValue)
      }
    } else {
      val ac = a.getXPathComparable(true, collator, implicitTimezone).asInstanceOf[Comparable[AnyRef]]
      val bc = b.getXPathComparable(true, collator, implicitTimezone).asInstanceOf[Comparable[AnyRef] with AnyRef]//ORBEON with AnyRef is ugly!
      if (ac == null || bc == null) {
        throw new ClassCastException("Values are not comparable (" + Type.displayTypeName(a) + 
          ", " + 
          Type.displayTypeName(b) + 
          ')')
      } else {
        ac.compareTo(bc)
      }
    }
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
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = compareAtomicValues(a, b) == 0
}

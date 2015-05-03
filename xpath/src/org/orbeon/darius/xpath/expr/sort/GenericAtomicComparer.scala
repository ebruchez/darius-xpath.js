// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.`type`.{AtomicType, Type}
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.value.{AtomicValue, CalendarValue, StringValue}

import scala.beans.BeanProperty

object GenericAtomicComparer {

  /**
   * Factory method to make a GenericAtomicComparer for values of known types
   * @param type0 primitive type of the first operand
   * @param type1 primitive type of the second operand
   * @param collator the collation to be used, if any. This is supplied as a NamedCollation object
   * which encapsulated both the collation URI and the collation itself.
   * @param implicitTimezone from the dynamic context
   * @return a GenericAtomicComparer for values of known types
   */
  def makeAtomicComparer(type0: AtomicType, 
      type1: AtomicType, 
      collator: StringCollator, 
      implicitTimezone: Int): AtomicComparer = {
    if (type0 == type1) {
      if (type0 == AtomicType.DATE_TIME || type0 == AtomicType.DATE || 
        type0 == AtomicType.TIME || 
        type0 == AtomicType.G_DAY || 
        type0 == AtomicType.G_MONTH || 
        type0 == AtomicType.G_MONTH_DAY || 
        type0 == AtomicType.G_YEAR || 
        type0 == AtomicType.G_YEAR_MONTH) {
        return new CalendarValueComparer(implicitTimezone)
      } else if (type0 == AtomicType.BOOLEAN || type0 == AtomicType.DAY_TIME_DURATION || 
        type0 == AtomicType.YEAR_MONTH_DURATION || 
        type0 == AtomicType.BASE64_BINARY || 
        type0 == AtomicType.HEX_BINARY || 
        type0 == AtomicType.QNAME) {
        return ComparableAtomicValueComparer.getInstance
      }
    }
    if (type0.isPrimitiveNumeric && type1.isPrimitiveNumeric) {
      return ComparableAtomicValueComparer.getInstance
    }
    if ((type0 == AtomicType.STRING || type0 == AtomicType.UNTYPED_ATOMIC || type0 == AtomicType.ANY_URI) &&
        (type1 == AtomicType.STRING || type1 == AtomicType.UNTYPED_ATOMIC || type1 == AtomicType.ANY_URI)) {
      if (collator.isInstanceOf[CodepointCollator]) {
        return CodepointCollatingComparer.getInstance
      } else {
        return new CollatingAtomicComparer(collator)
      }
    }
    new GenericAtomicComparer(collator, implicitTimezone)
  }
}

/**
 * An AtomicComparer used for comparing atomic values of arbitrary item types. It encapsulates
 * a Collator that is used when the values to be compared are strings. It also supports
 * a separate method for testing equality of items, which can be used for data types that
 * are not ordered.
 *
 * @author Michael H. Kay
 *
 */
class GenericAtomicComparer(@BeanProperty var collator: StringCollator, var implicitTimezone: Int)
    extends AtomicComparer {

  if (collator == null) {
    this.collator = CodepointCollator.getInstance
  }

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
   * @return <0 if a < b, 0 if a = b, >0 if a > b
   * @throws ClassCastException if the objects are not comparable
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      return if (b == null) 0 else -1
    } else if (b == null) {
      return +1
    }
    if (a.isInstanceOf[StringValue] && b.isInstanceOf[StringValue]) {
      collator match {
        case codepointCollator: CodepointCollator ⇒
          codepointCollator.compareCS(a.getStringValue, b.getStringValue)
        case _ ⇒
          collator.compareStrings(a.getStringValue, b.getStringValue)
      }
    } else {
      val ac = a.getXPathComparable(ordered = true, collator, implicitTimezone).asInstanceOf[Comparable[AnyRef]]
      val bc = b.getXPathComparable(ordered = true, collator, implicitTimezone).asInstanceOf[Comparable[AnyRef] with AnyRef]//ORBEON with AnyRef is ugly!
      if (ac == null || bc == null) {
        throw new ClassCastException("Objects are not comparable (" + Type.displayTypeName(a) + 
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
   * values are compared as if they were strings; if different semantics are wanted, the conversion
   * must be done by the caller.
   *
   * @param a the first object to be compared. If it is a StringValue, the
   * collator is used to compare the values, otherwise the value must implement the equals() method.
   * @param b the second object to be compared. This must be comparable with the first object: for
   * example, if one is a string, they must both be strings.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = {
    a match {
      case _: StringValue if b.isInstanceOf[StringValue] ⇒
        val result = collator.comparesEqual(a.getStringValue, b.getStringValue)
        //println(result)
        result
      case calendarValue: CalendarValue if b.isInstanceOf[CalendarValue] ⇒
        calendarValue.compareTo(b.asInstanceOf[CalendarValue], implicitTimezone) == 0
      case _ ⇒
        val ac = a.getXPathComparable(ordered = false, collator, implicitTimezone)
        val bc = b.getXPathComparable(ordered = false, collator, implicitTimezone)
        ac == bc
    }
  }
}

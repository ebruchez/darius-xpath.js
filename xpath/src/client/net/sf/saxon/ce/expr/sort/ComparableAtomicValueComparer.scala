// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.value.AtomicValue

object ComparableAtomicValueComparer {

  private val THE_INSTANCE: ComparableAtomicValueComparer = new ComparableAtomicValueComparer()

  /**
   * Get the singleton instance of this class
   * @return  the singleton instance of this class
   */
  def getInstance(): ComparableAtomicValueComparer = THE_INSTANCE
}

/**
 * A comparer for comparing two "ordinary" atomic values, where the values implement the Comparable
 * interface and the equals() method with the appropriate XPath semantics. This rules out use of
 * collations, conversion of untyped atomic values, and context dependencies such as implicit timezone.
 */
class ComparableAtomicValueComparer protected () extends AtomicComparer {

  def getCollator(): StringCollator = null

  /**
   * Compare two AtomicValue objects according to the rules for their data type. UntypedAtomic
   * values are compared as if they were strings; if different semantics are wanted, the conversion
   * must be done by the caller.
   *
   *
   * @param a the first object to be compared. This must be an AtomicValue and it must implement
   * Comparable with context-free XPath comparison semantics
   * @param b the second object to be compared. This must be an AtomicValue and it must implement
   * Comparable with context-free XPath comparison semantics
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not comparable
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      return if (b == null) 0 else -1
    } else if (b == null) {
      return +1
    }
    a.asInstanceOf[Comparable[AnyRef]].compareTo(b)
  }

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   * @param a the first object to be compared. This must be an AtomicValue and it must implement
   * equals() with context-free XPath comparison semantics
   * @param b the second object to be compared. This must be an AtomicValue and it must implement
   * equals() with context-free XPath comparison semantics
   * @return true if the values are equal, false if not
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = a == b
}

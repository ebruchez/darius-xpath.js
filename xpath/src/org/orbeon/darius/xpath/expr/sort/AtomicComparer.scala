// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.value.AtomicValue

/**
 * Interface representing an object that can be used to compare two XPath atomic values for equality or
 * for ordering.
 */
trait AtomicComparer {

  /**
   * Get the collation used by this AtomicComparer if any
   * @return the collation used for comparing strings, or null if not applicable
   */
  def getCollator: StringCollator

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
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   * @param a the first object to be compared.
   * @param b the second object to be compared.
   * @return true if the values are equal, false if not
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean
}

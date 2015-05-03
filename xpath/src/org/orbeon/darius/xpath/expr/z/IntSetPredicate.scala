// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.z

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
  def getIntSet: IntSet = set
}

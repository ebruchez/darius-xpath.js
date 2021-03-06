// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.z

/**
 * An IntPredicate formed as the intersection of two other predicates: it matches
 * an integer if both of the operands matches the integer
 */
class IntIntersectionPredicate(var p1: IntPredicate, var p2: IntPredicate) extends IntPredicate {

  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  def matches(value: Int): Boolean = p1.matches(value) && p2.matches(value)

  /**
   * Get the operands
   * @return an array containing the two operands
   */
  def getOperands: Array[IntPredicate] = Array(p1, p2)
}

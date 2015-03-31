// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

/**
 * An IntPredicate formed as the complement of another predicate;
 * it matches an integer if the operand does not, and vice versa.
 */
class IntComplementPredicate(var p1: IntPredicate) extends IntPredicate {

  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  def matches(value: Int): Boolean = !p1.matches(value)

  /**
   * Get the operand
   * @return the negated predicate
   */
  def getOperand: IntPredicate = p1
}

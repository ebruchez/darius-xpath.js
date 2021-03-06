// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.orbeon.Util

/**
 * A StringCollator that sorts lowercase before uppercase, or vice versa.
 *
 * <p>Case is irrelevant, unless the strings are equal ignoring
 * case, in which case lowercase comes first.</p>
 *
 * @author Michael H. Kay
 */
class CaseFirstCollator(base: StringCollator, var upperFirst: Boolean) extends StringCollator {

  private val baseCollator: StringCollator = base

  /**
   * Compare two string objects: case is irrelevant, unless the strings are equal ignoring
   * case, in which case lowercase comes first.
   *
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are of the wrong type for this Comparer
   */
  def compareStrings(a: String, b: String): Int = {
    val diff = baseCollator.compareStrings(a, b)
    if (diff != 0) {
      return diff
    }
    var i = 0
    var j = 0
    while (true) {
      while (i < a.length && j < b.length && a.charAt(i) == b.charAt(j)) {
        i += 1
        j += 1
      }
      while (i < a.length && ! Util.isLetter(a.charAt(i))) {
        i += 1
      }
      while (j < b.length && ! Util.isLetter(b.charAt(j))) {
        j += 1
      }
      if (i >= a.length) {
        return 0
      }
      if (j >= b.length) {
        return 0
      }
      val aFirst = if (upperFirst) Character.isUpperCase(a.charAt(i)) else Character.isLowerCase(a.charAt(i))
      i += 1
      val bFirst = if (upperFirst) Character.isUpperCase(b.charAt(j)) else Character.isLowerCase(b.charAt(j))
      j += 1
      if (aFirst && !bFirst) {
        return -1
      }
      if (bFirst && !aFirst) {
        return +1
      }
    }
    throw new IllegalStateException
  }

  /**
   * Compare two strings for equality. This may be more efficient than using compareStrings and
   * testing whether the result is zero, but it must give the same result
   * @param s1 the first string
   * @param s2 the second string
   * @return true if and only if the strings are considered equal,
   */
  def comparesEqual(s1: String, s2: String): Boolean = compareStrings(s1, s2) == 0

  /**
   * Get a collation key for two Strings. The essential property of collation keys
   * is that if two values are equal under the collation, then the collation keys are
   * compare correctly under the equals() method.
   */
  def getCollationKey(s: String): AnyRef = baseCollator.getCollationKey(s)
}

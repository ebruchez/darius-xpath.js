// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.lib.StringCollator

object CodepointCollator {

  private val theInstance: CodepointCollator = new CodepointCollator()

  def getInstance(): CodepointCollator = theInstance
}

/**
 * A collating sequence that uses Unicode codepoint ordering
 */
class CodepointCollator extends StringCollator {

  /**
   * Compare two string objects.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are of the wrong type for this Comparer
   */
  def compareStrings(a: String, b: String): Int = compareCS(a, b)

  /**
   * Compare two CharSequence objects. This is hand-coded to avoid converting the objects into
   * Strings.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are of the wrong type for this Comparer
   */
  def compareCS(a: CharSequence, b: CharSequence): Int = {
    val alen = a.length
    val blen = b.length
    var i = 0
    var j = 0
    while (true) {
      if (i == alen) {
        if (j == blen) {
          return 0
        } else {
          return -1
        }
      }
      if (j == blen) {
        return +1
      }
      var nexta = a.charAt(i).toInt
      i += 1
      if (nexta >= 55296 && nexta <= 56319) {
        nexta = ((nexta - 55296) * 1024) + (a.charAt(i).toInt - 56320) + 65536
        i += 1
      }
      var nextb = b.charAt(j).toInt
      j += 1
      if (nextb >= 55296 && nextb <= 56319) {
        nextb = ((nextb - 55296) * 1024) + (b.charAt(j).toInt - 56320) + 65536
        j += 1
      }
      val c = nexta - nextb
      if (c != 0) {
        return c
      }
    }
    throw new IllegalStateException
  }

  /**
   * Test whether one string is equal to another, according to the rules
   * of the XPath compare() function. The result is true if and only if the
   * compare() method returns zero: but the implementation may be more efficient
   * than calling compare and testing the result for zero
   *
   * @param s1 the first string
   * @param s2 the second string
   * @return true iff s1 equals s2
   */
  def comparesEqual(s1: String, s2: String): Boolean = s1 == s2

  /**
   * Test whether one string contains another, according to the rules
   * of the XPath contains() function
   *
   * @param s1 the containing string
   * @param s2 the contained string
   * @return true iff s1 contains s2
   */
  def contains(s1: String, s2: String): Boolean = s1.indexOf(s2) >= 0

  /**
   * Test whether one string starts with another, according to the rules
   * of the XPath starts-with() function
   *
   * @param s1 the containing string
   * @param s2 the contained string
   * @return true iff s1 starts with s2
   */
  def startsWith(s1: String, s2: String): Boolean = s1.startsWith(s2)

  /**
   * Get a collation key for two Strings. The essential property of collation keys
   * is that if two values are equal under the collation, then the collation keys are
   * compare correctly under the equals() method.
   */
  def getCollationKey(s: String): AnyRef = s
}

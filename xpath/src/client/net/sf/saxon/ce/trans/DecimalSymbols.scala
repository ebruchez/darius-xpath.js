// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import java.util.Arrays

import client.net.sf.saxon.ce.orbeon.HashMap
import client.net.sf.saxon.ce.trans.DecimalSymbols._

object DecimalSymbols {

  var zeroDigits: Array[Int] = Array(0x0030, 0x0660, 0x06f0, 0x0966, 0x09e6, 0x0a66, 0x0ae6, 0x0b66, 0x0be6, 0x0c66, 0x0ce6, 0x0d66, 0x0e50, 0x0ed0, 0x0f20, 0x1040, 0x17e0, 0x1810, 0x1946, 0x19d0, 0xff10, 0x104a0, 0x1d7ce, 0x1d7d8, 0x1d7e2, 0x1d7ec, 0x1d7f6)
}

/**
 * This class is modelled on Java's DecimalFormatSymbols, but it allows the use of any
 * Unicode character to represent symbols such as the decimal point and the grouping
 * separator, whereas DecimalFormatSymbols restricts these to a char (1-65535). Since
 * this is essentially a data structure with no behaviour, we don't bother with getter
 * and setter methods but just expose the fields
 */
class DecimalSymbols {

  var decimalSeparator: Int = '.'

  var groupingSeparator: Int = ','

  var digit: Int = '#'

  var minusSign: Int = '-'

  var percent: Int = '%'

  var permill: Int = '‰'

  var zeroDigit: Int = '0'

  var patternSeparator: Int = ';'

  var infinity: String = "Infinity"

  var NaN: String = "NaN"

  /**
   * Check that no character is used in more than one role
   * @throws XPathException if a character is used in more than one role
   */
  def checkDistinctRoles(): Unit = {
    val map = new HashMap[Int, String](20)
    map.put(decimalSeparator, "decimal-separator")
    if (map.get(groupingSeparator) != null) {
      duplicate("grouping-separator", map.get(groupingSeparator))
    }
    map.put(groupingSeparator, "grouping-separator")
    if (map.get(percent) != null) {
      duplicate("percent", map.get(percent))
    }
    map.put(percent, "percent")
    if (map.get(permill) != null) {
      duplicate("per-mille", map.get(permill))
    }
    map.put(permill, "per-mille")
    if (map.get(zeroDigit) != null) {
      duplicate("zero-digit", map.get(zeroDigit))
    }
    map.put(zeroDigit, "zero-digit")
    if (map.get(digit) != null) {
      duplicate("digit", map.get(digit))
    }
    map.put(digit, "digit")
    if (map.get(patternSeparator) != null) {
      duplicate("pattern-separator", map.get(patternSeparator))
    }
  }

  /**
   * Report that a character is used in more than one role
   * @param role1  the first role
   * @param role2  the second role
   * @throws XPathException (always)
   */
  private def duplicate(role1: String, role2: String): Unit = {
    throw new XPathException("The same character is used as the " + role1 + " and as the " + 
      role2)
  }

  /**
   * Check that the character declared as a zero-digit is indeed a valid zero-digit
   * @return false if it is not a valid zero-digit
   */
  def isValidZeroDigit(): Boolean = {
    Arrays.binarySearch(zeroDigits, zeroDigit) >= 0
  }

  /**
   * Test if two sets of decimal format symbols are the same
   * @param obj the other set of symbols
   * @return true if the same characters/strings are assigned to each role in both sets of symbols
   */
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[DecimalSymbols]) {
      return false
    }
    val o = obj.asInstanceOf[DecimalSymbols]
    decimalSeparator == o.decimalSeparator && groupingSeparator == o.groupingSeparator && 
      digit == o.digit && 
      minusSign == o.minusSign && 
      percent == o.percent && 
      permill == o.permill && 
      zeroDigit == o.zeroDigit && 
      patternSeparator == o.patternSeparator && 
      infinity == o.infinity && 
      NaN == o.NaN
  }

  override def hashCode(): Int = {
    decimalSeparator + (37 * groupingSeparator) + (41 * digit)
  }
}

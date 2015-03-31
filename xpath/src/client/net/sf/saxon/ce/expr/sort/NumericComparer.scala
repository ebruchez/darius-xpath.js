// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.`type`.StringToDouble
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.value._

object NumericComparer {

  private val THE_INSTANCE: NumericComparer = new NumericComparer()

  def getInstance(): NumericComparer = THE_INSTANCE
}

/**
 * A Comparer used for comparing sort keys when data-type="number". The items to be
 * compared are converted to numbers, and the numbers are then compared directly. NaN values
 * compare equal to each other, and equal to an empty sequence, but less than anything else.
 * <p/>
 * This class is used in XSLT only, so there is no need to handle XQuery's "empty least" vs
 * "empty greatest" options.
 *
 * @author Michael H. Kay
 *
 */
class NumericComparer protected () extends AtomicComparer {

  def getCollator(): StringCollator = null

  /**
   * Compare two Items by converting them to numbers and comparing the numeric values. If either
   * value cannot be converted to a number, it is treated as NaN, and compares less that the other
   * (two NaN values compare equal).
   *
   * @param a the first Item to be compared.
   * @param b the second Item to be compared.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not Items
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    var d1: Double = 0.0
    var d2: Double = 0.0
    d1 = makeDouble(a)
    d2 = makeDouble(b)
    if (d1.isNaN) {
      if (d2.isNaN) {
        return 0
      } else {
        return -1
      }
    }
    if (d2.isNaN) {
      return +1
    }
    if (d1 < d2) return -1
    if (d1 > d2) return +1
    0
  }

  private def makeDouble(a: AtomicValue): Double = {
    var d1: Double = 0.0
    if (a.isInstanceOf[NumericValue]) {
      d1 = a.asInstanceOf[NumericValue].getDoubleValue
    } else if (a == null) {
      d1 = Double.NaN
    } else {
      try {
        d1 = StringToDouble.stringToNumber(a.getStringValue)
      } catch {
        case err: NumberFormatException ⇒ d1 = Double.NaN
      }
    }
    d1
  }

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   *
   * @param a the first object to be compared.
   * @param b the second object to be compared.
   * @return true if the values are equal, false if not
   * @throws ClassCastException if the objects are not comparable
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = compareAtomicValues(a, b) == 0
}

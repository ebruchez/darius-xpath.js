// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType}
import client.net.sf.saxon.ce.value.{AtomicValue, NumericValue, UntypedAtomicValue}

/**
 * Expression that performs numeric promotion to xs:double
 */
class PromoteToDouble(exp: Expression) extends NumericPromoter(exp) {

  /**
   * Determine the data type of the items returned by the expression, if possible
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   * or Type.ITEM (meaning not known in advance)
   */
  override def getItemType: ItemType = AtomicType.DOUBLE

  /**
   * Perform the promotion
   * @param value the numeric or untyped atomic value to be promoted
   * @return the value that results from the promotion
   */
  protected def promote(value: AtomicValue): AtomicValue = {
    if (!(value.isInstanceOf[NumericValue] || value.isInstanceOf[UntypedAtomicValue])) {
      typeError("Cannot promote non-numeric value to xs:double", "XPTY0004")
    }
    value.convert(AtomicType.DOUBLE).asAtomic()
  }
}

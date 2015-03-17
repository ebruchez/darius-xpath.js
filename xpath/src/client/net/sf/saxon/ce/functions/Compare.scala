// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.{AtomicValue, IntegerValue}

class Compare extends CollatingFunction {

  override def newInstance(): Compare = new Compare()

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg0 == null) {
      return null
    }
    val arg1 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg1 == null) {
      return null
    }
    val collator = getAtomicComparer(2, context)
    val result = collator.compareAtomicValues(arg0, arg1)
    if (result < 0) {
      IntegerValue.MINUS_ONE
    } else if (result > 0) {
      IntegerValue.PLUS_ONE
    } else {
      IntegerValue.ZERO
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.{AtomicValue, BooleanValue}

/**
 * XPath 2.0 codepoint-equal() function.
 * Compares two strings using the unicode codepoint collation. (The function was introduced
 * specifically to allow URI comparison: URIs are promoted to strings when necessary.)
 */
class CodepointEqual extends SystemFunction {

  def newInstance(): CodepointEqual = new CodepointEqual()

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val op1 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (op1 == null) {
      return null
    }
    val op2 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    if (op2 == null) {
      return null
    }
    BooleanValue.get(op1.getStringValue == op2.getStringValue)
  }
}

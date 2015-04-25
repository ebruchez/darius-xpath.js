// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.ForceCase._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.{AtomicValue, StringValue}

object ForceCase {
  val UPPERCASE = 0
  val LOWERCASE = 1
}

/**
 * This class implements the upper-case() and lower-case() functions
 */
class ForceCase(_operation: Int) extends SystemFunction {

  this.operation = _operation

  def newInstance(): ForceCase = new ForceCase(operation)

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = {
    val sv = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    if (sv == null) {
      return StringValue.EMPTY_STRING
    }
    var s = sv.getStringValue
    s = if (operation == UPPERCASE) s.toUpperCase else s.toLowerCase
    StringValue.makeStringValue(s)
  }
}

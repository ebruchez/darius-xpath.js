// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.{Item, NodeInfo}
import client.net.sf.saxon.ce.value.AnyURIValue

/**
 * This class supports the base-uri() function in XPath 2.0
 */
class BaseURI extends SystemFunction {

  def newInstance(): BaseURI = new BaseURI()

  /**
   * Evaluate the function at run-time
   */
  override def evaluateItem(c: XPathContext): Item = {
    val node = argument(0).evaluateItem(c).asInstanceOf[NodeInfo]
    if (node == null) {
      return null
    }
    val s = node.getBaseURI
    if (s == null) {
      return null
    }
    new AnyURIValue(s)
  }
}

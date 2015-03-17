// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.{Item, NodeInfo}

/**
 * Implement the XPath 2.0 root() function
 */
class Root extends SystemFunction {

  def newInstance(): Root = new Root()

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-significant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    var prop = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.NON_CREATIVE
    if ((getNumberOfArguments == 0) || 
      (argument(0).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = {
    val start = argument(0).evaluateItem(c).asInstanceOf[NodeInfo]
    if (start == null) {
      return null
    }
    start.getRoot
  }
}

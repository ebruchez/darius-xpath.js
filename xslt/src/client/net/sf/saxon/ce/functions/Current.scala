// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.trans.XPathException
import Current._

import scala.collection.JavaConversions._

object Current {

  /**
   * The name of the Current function
   */
  val FN_CURRENT = new StructuredQName("", NamespaceConstant.FN, "current")
}

/**
 * Implement the XSLT current() function
 */
class Current extends SystemFunction {

  def newInstance(): Current = new Current()

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  def computeSpecialProperties(): Int = {
    StaticProperty.CONTEXT_DOCUMENT_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.ORDERED_NODESET | 
      StaticProperty.NON_CREATIVE
  }

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    throw new AssertionError("current() function should have been rewritten at compile time")
  }

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies: Int = {
    StaticProperty.DEPENDS_ON_CURRENT_ITEM | StaticProperty.DEPENDS_ON_LOCAL_VARIABLES
  }
}

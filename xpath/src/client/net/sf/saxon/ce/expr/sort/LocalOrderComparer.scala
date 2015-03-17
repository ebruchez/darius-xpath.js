// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.NodeInfo

import scala.beans.BeanProperty

object LocalOrderComparer {

  @BeanProperty
  var instance: LocalOrderComparer = new LocalOrderComparer()
}

/**
 * A Comparer used for comparing nodes in document order. This
 * comparer assumes that the nodes being compared come from the same document
 *
 * @author Michael H. Kay
 *
 */
class LocalOrderComparer extends NodeOrderComparer {

  def compare(a: NodeInfo, b: NodeInfo): Int = {
    val n1 = a.asInstanceOf[NodeInfo]
    val n2 = b.asInstanceOf[NodeInfo]
    n1.compareOrder(n2)
  }
}

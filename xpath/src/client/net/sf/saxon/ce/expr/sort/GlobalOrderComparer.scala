// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.orbeon.Util

import scala.beans.BeanProperty

object GlobalOrderComparer {

  @BeanProperty
  var instance: GlobalOrderComparer = new GlobalOrderComparer()
}

/**
 * A Comparer used for comparing nodes in document order. This
 * comparer is used when there is no guarantee that the nodes being compared
 * come from the same document
 *
 * @author Michael H. Kay
 *
 */
class GlobalOrderComparer extends NodeOrderComparer {

  def compare(a: NodeInfo, b: NodeInfo): Int = {
    if (a == b) {
      return 0
    }
    val d1 = a.getDocumentNumber
    val d2 = b.getDocumentNumber
    if (d1 == d2) {
      return a.compareOrder(b)
    }
    Util.signum(d1 - d2)
  }
}

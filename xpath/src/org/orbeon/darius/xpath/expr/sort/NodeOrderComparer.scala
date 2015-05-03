// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.om.NodeInfo

/**
 * A Comparer used for comparing nodes in document order
 *
 * @author Michael H. Kay
 *
 */
trait NodeOrderComparer {

  /**
   * Compare two objects.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   */
  def compare(a: NodeInfo, b: NodeInfo): Int
}

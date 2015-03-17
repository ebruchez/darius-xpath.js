// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans.update

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.trans.XPathException
import com.google.gwt.dom.client.Node
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A pending update action, such as is found on a pending update list
 */
abstract class PendingUpdateAction {

  /**
   * Apply the pending update action to the affected nodes
   * @param context the XPath evaluation context
   * @throws XPathException if any error occurs applying the update
   */
  def apply(context: XPathContext): Unit

  /**
   * Get the target node of the update action
   * @return the target node, the node to which this update action applies. Returns null in the
   * case of a delete action, which affects multiple nodes.
   */
  def getTargetNode(): Node
}

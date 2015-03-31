// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans.update

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.trans.XPathException
import com.google.gwt.dom.client.Node
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A pending update action representing the effect of a delete expression
 */
class DeleteAction(@BeanProperty var targetNode: Node) extends PendingUpdateAction {

  /**
   * Apply the pending update action to the affected node
   *
   * @param context the XPath evaluation context
   */
  def apply(context: XPathContext): Unit = {
    val parent = targetNode.getParentElement
    if (parent != null) {
      parent.removeChild(targetNode)
    }
  }
}

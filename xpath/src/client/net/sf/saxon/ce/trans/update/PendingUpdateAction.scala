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

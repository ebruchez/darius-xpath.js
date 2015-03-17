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
  def apply(context: XPathContext) {
    val parent = targetNode.getParentElement
    if (parent != null) {
      parent.removeChild(targetNode)
    }
  }
}

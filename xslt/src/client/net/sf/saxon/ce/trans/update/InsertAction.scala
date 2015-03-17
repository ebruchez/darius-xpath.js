// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans.update

import client.net.sf.saxon.ce.expr.XPathContext
import com.google.gwt.dom.client.Node
import com.google.gwt.dom.client.NodeList
import InsertAction._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object InsertAction {

  val FIRST = 0

  val LAST = 1

  val BEFORE = 2

  val AFTER = 3
}

/**
 * A pending update action representing the effect of an insert expression
 */
class InsertAction(var content: Node, @BeanProperty var targetNode: Node, var position: Int)
    extends PendingUpdateAction {

  /**
   * Apply the pending update action to the affected nodes
   *
   * @param context the XPath evaluation context
   */
  def apply(context: XPathContext) position match {
    case FIRST => {
      val list = content.getChildNodes
      val count = list.getLength
      var i = count - 1
      while (i >= 0) {
        targetNode.insertFirst(list.getItem(i))
        i -= 1
      }
      //break
    }
    case LAST => {
      while (content.hasChildNodes()) {
        targetNode.appendChild(content.getFirstChild)
      }
      //break
    }
    case BEFORE => {
      val refNode = targetNode.getChild(position)
      val list = content.getChildNodes
      val count = list.getLength
      for (i <- 0 until count) {
        targetNode.insertBefore(list.getItem(i), refNode)
      }
      //break
    }
    case AFTER => {
      val refNode = targetNode.getChild(position)
      val list = content.getChildNodes
      val count = list.getLength
      var i = count - 1
      while (i >= 0) {
        targetNode.insertAfter(list.getItem(i), refNode)
        i -= 1
      }
      //break
    }
    case _ => throw new UnsupportedOperationException("Unknown insert position " + position)
  }
}

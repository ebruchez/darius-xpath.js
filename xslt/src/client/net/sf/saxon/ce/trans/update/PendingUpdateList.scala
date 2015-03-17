// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans.update

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import java.util.ArrayList
import java.util.List
import java.util.logging.Logger
//remove if not needed
import scala.collection.JavaConversions._

/**
 * List of pending updates to the browser's HTML page
 */
class PendingUpdateList(config: Configuration) {

  private var list: List[PendingUpdateAction] = new ArrayList[PendingUpdateAction]()

  private var logger: Logger = Logger.getLogger("PendingUpdateList")

  /**
   * Add an action to the pending update list
   * @param action the Pending Update Action to be added to the list
   * @throws client.net.sf.saxon.ce.trans.XPathException if the pending update action conflicts with an action that is already on the list
   */
  def add(action: PendingUpdateAction) {
    list.add(action)
  }

  /**
   * Apply the pending updates
   * @param context the XPath dynamic evaluation context
   */
  def apply(context: XPathContext) {
    synchronized {
      var state = ""
      try {
        state = "delete"
        for (i <- 0 until list.size) {
          val action = list.get(i)
          if (action.isInstanceOf[DeleteAction]) {
            action.apply(context)
          }
        }
        state = "insert"
        for (i <- 0 until list.size) {
          val action = list.get(i)
          if (action.isInstanceOf[InsertAction]) {
            action.apply(context)
          }
        }
        state = "set-attribute"
        for (i <- 0 until list.size) {
          val action = list.get(i)
          if (action.isInstanceOf[SetAttributeAction]) {
            action.apply(context)
          }
        }
        list = new ArrayList[PendingUpdateAction]()
      } catch {
        case e: Exception => {
          logger.severe("Error on DOM write action: " + state + " " + e.getMessage)
          throw new XPathException(e)
        }
      }
    }
  }
}

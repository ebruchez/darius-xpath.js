// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.linked

import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.event.Receiver
import org.orbeon.darius.xpath.value.AtomicValue
import org.orbeon.darius.xpath.value.StringValue

/**
 * CommentImpl is an implementation of a Comment node
 * @author Michael H. Kay
 */
class CommentImpl(content: String) extends NodeImpl {

  var comment: String = content

  def getStringValue: String = comment

  /**
   * Get the typed value of this node.
   * Returns the string value, as an instance of xs:string
   */
  override def getTypedValue: AtomicValue = new StringValue(getStringValue)

  def getNodeKind: Int = Type.COMMENT

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int): Unit = {
    out.comment(comment)
  }
}

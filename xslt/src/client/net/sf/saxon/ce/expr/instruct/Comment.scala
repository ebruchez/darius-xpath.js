// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction representing an xsl:comment element in the stylesheet.
 */
class Comment extends SimpleNodeConstructor {

  def getItemType(): ItemType = NodeKindTest.COMMENT

  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Unit = {
  }

  /**
   * Process the value of the node, to create the new node.
   * @param value the string value of the new node
   * @param context the dynamic evaluation context
   * @throws XPathException
   */
  def processValue(value: CharSequence, context: XPathContext): Unit = {
    val comment = checkContent(value.toString, context)
    val out = context.getReceiver
    out.comment(comment)
  }

  /**
   * Check the content of the node, and adjust it if necessary
   *
   * @param comment    the supplied content
   * @param context the dynamic context
   * @return the original content, unless adjustments are needed
   * @throws XPathException if the content is invalid
   */
  protected def checkContent(comment: String, context: XPathContext): String = {
    while (true) {
      val hh = comment.indexOf("--")
      if (hh < 0) //break
      comment = comment.substring(0, hh + 1) + ' ' + comment.substring(hh + 1)
    }
    if (comment.length > 0 && comment.charAt(comment.length - 1) == '-') {
      comment = comment + ' '
    }
    comment
  }
}

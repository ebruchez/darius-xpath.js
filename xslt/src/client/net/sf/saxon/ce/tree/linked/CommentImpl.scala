package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * CommentImpl is an implementation of a Comment node
 * @author Michael H. Kay
 */
class CommentImpl(content: String) extends NodeImpl {

  var comment: String = content

  def getStringValue(): String = comment

  /**
   * Get the typed value of this node.
   * Returns the string value, as an instance of xs:string
   */
  def getTypedValue(): AtomicValue = new StringValue(getStringValue)

  def getNodeKind(): Int = Type.COMMENT

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int) {
    out.comment(comment)
  }
}

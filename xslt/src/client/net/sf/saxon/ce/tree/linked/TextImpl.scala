package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A node in the XML parse tree representing character content<P>
 * @author Michael H. Kay
 */
class TextImpl(var content: String) extends NodeImpl {

  /**
   * Append to the content of the text node
   * @param content the new content to be appended
   */
  def appendStringValue(content: String) {
    this.content = this.content + content
  }

  /**
   * Return the character value of the node.
   * @return the string value of the node
   */
  def getStringValue(): String = content

  /**
   * Return the type of node.
   * @return Type.TEXT
   */
  def getNodeKind(): Int = Type.TEXT

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int) {
    out.characters(content)
  }
}

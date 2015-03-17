package client.net.sf.saxon.ce.dom

import com.google.gwt.xml.client.Node
import java.util.List

/**
 * This class wraps a list of nodes as a DOM NodeList
 */
class DOMNodeList(var sequence: List[Node]) extends com.google.gwt.xml.client.NodeList {

  /**
   * return the number of nodes in the list (DOM method)
   */
  def getLength(): Int = sequence.size

  /**
   * Return the n'th item in the list (DOM method)
   * @throws java.lang.ClassCastException if the item is not a DOM Node
   */
  def item(index: Int): Node = {
    if (index < 0 || index >= sequence.size) {
      null
    } else {
      sequence.get(index).asInstanceOf[Node]
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.dom

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

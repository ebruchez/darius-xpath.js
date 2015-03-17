// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.om.{Item, NodeInfo}

/**
 * An iterator over nodes, that prepends a given node to the nodes
 * returned by another iterator. Used to modify an iterator over axis A
 * to one that iterates over A-OR-SELF.
 */
class PrependIterator(var start: NodeInfo, var base: UnfailingIterator) extends UnfailingIterator {

  var position: Int = 0

  /**
   * Get the next item in the sequence. <BR>
   *
   * @return the next Item. If there are no more nodes, return null.
   */
  def next(): Item = {
    if (position == 0) {
      position = 1
      start
    } else {
      base.next()
    }
  }

  /**
   * Get another iterator over the same sequence of items, positioned at the
   * start of the sequence
   *
   * @return a new iterator over the same sequence
   */
  def getAnother(): UnfailingIterator = {
    new PrependIterator(start, base.getAnother)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.{Expression, XPathContext}
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}

/**
 * Iterate over the instructions in the Block, concatenating the result of each instruction
 * into a single combined sequence.
 */
class BlockIterator(var children: Array[Expression], var context: XPathContext)
    extends SequenceIterator {

  private var i: Int = 0

  private var child: SequenceIterator = _

  private var current: Item = _

  private var position: Int = 0

  /**
   * Get the next item in the sequence. <BR>
   *
   * @return the next item, or null if there are no more items.
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error occurs retrieving the next item
   */
  def next(): Item = {
    if (position < 0) {
      return null
    }
    while (true) {
      if (child == null) {
        child = children(i).iterate(context)
        i += 1
      }
      current = child.next()
      if (current != null) {
        position += 1
        return current
      }
      child = null
      if (i >= children.length) {
        current = null
        position = -1
        return null
      }
    }
    throw new IllegalStateException
  }

  /**
   * Get another SequenceIterator that iterates over the same items as the original,
   * but which is repositioned at the start of the sequence.
   *
   * @return a SequenceIterator that iterates over the same items,
   *         positioned before the first item
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any error occurs
   */
  def getAnother: SequenceIterator = new BlockIterator(children, context)
}

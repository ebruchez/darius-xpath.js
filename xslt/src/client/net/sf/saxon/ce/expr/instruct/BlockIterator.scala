package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

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
        child = children(i += 1).iterate(context)
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
        null
      }
    }
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
  def getAnother(): SequenceIterator = new BlockIterator(children, context)
}

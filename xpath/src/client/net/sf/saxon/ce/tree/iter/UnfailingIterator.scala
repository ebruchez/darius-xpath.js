package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.om.Item
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A SequenceIterator is used to iterate over a sequence. An UnfailingIterator
 * is a SequenceIterator that throws no checked exceptions.
 */
trait UnfailingIterator extends SequenceIterator {

  /**
   * Get the next item in the sequence. <BR>
   * @return the next Item. If there are no more nodes, return null.
   */
  def next(): Item

  /**
   * Get another iterator over the same sequence of items, positioned at the
   * start of the sequence. It must be possible to call this method at any time, whether
   * none, some, or all of the items in the original iterator have been read. The method
   * is non-destructive: it does not change the state of the original iterator.
   * @return a new iterator over the same sequence
   */
  def getAnother(): UnfailingIterator
}

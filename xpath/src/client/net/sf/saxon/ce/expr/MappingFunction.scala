package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * MappingFunction is an interface that must be satisfied by an object passed to a
 * MappingIterator. It represents an object which, given an Item, can return a
 * SequenceIterator that delivers a sequence of zero or more Items.
 */
trait MappingFunction {

  /**
   * Map one item to a sequence.
   * @param item The item to be mapped.
   * @return one of the following: (a) a SequenceIterator over the sequence of items that the supplied input
   * item maps to, or (b) null if it maps to an empty sequence.
   */
  def map(item: Item): SequenceIterator
}

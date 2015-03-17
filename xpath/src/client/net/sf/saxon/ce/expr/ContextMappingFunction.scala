package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ContextMappingFunction is an interface that must be satisfied by an object passed to a
 * ContextMappingIterator. It represents an object which, given an Item, can return a
 * SequenceIterator that delivers a sequence of zero or more Items.
 * <p>
 * This is a specialization of the more general MappingFunction class: it differs in that
 * each item being processed becomes the context item while it is being processed.
 */
trait ContextMappingFunction {

  /**
   * Map one item to a sequence.
   * @param context The processing context. The item to be mapped is the context item identified
   * from this context: the values of position() and last() also relate to the set of items being mapped
   * @return a SequenceIterator over the sequence of items that the supplied input
   * item maps to
   */
  def map(context: XPathContext): SequenceIterator
}

package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * MappingFunction is an interface that must be satisfied by an object passed to a
 * MappingIterator. StatefulMappingFunction is a sub-interface representing a mapping
 * function that maintains state information, and which must therefore be cloned
 * when the mapping iterator is cloned.
 */
trait StatefulMappingFunction {

  /**
   * Return a clone of this MappingFunction, with the state reset to its state at the beginning
   * of the underlying iteration
   * @return a clone of this MappingFunction
   * @param newBaseIterator the cloned iterator to which the mapping function will be applied
   */
  def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction
}

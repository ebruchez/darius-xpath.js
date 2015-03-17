package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.expr.LastPositionFinder
import client.net.sf.saxon.ce.om.Sequence
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This interface is an extension to the SequenceIterator interface; it represents
 * a SequenceIterator that is based on an in-memory representation of a sequence,
 * and that is therefore capable of returned a SequenceValue containing all the items
 * in the sequence.
 *
 * <p>GroundedIterator extends LastPositionFinder because any sequence that can
 * be readily materialized can also determine how many items are contained; the reverse
 * is not always true.</p>
 */
trait GroundedIterator extends LastPositionFinder {

  /**
   * Return a Value containing all the items in the sequence returned by this
   * SequenceIterator. This should involve no computation, and throws no errors.
   * @return the corresponding Value, or null if the value is not known
   */
  def materialize(): Sequence
}

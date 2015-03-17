package client.net.sf.saxon.ce.expr.z

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An iterator over a sequence of unboxed int values
 */
trait IntIterator {

  /**
   * Test whether there are any more integers in the sequence
   * @return true if there are more integers to come
   */
  def hasNext(): Boolean

  /**
   * Return the next integer in the sequence. The result is undefined unless hasNext() has been called
   * and has returned true.
   * @return the next integer in the sequence
   */
  def next(): Int
}

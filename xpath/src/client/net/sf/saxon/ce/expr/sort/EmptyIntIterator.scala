package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr.z.IntIterator
import EmptyIntIterator._
//remove if not needed
import scala.collection.JavaConversions._

object EmptyIntIterator {

  private var THE_INSTANCE: EmptyIntIterator = new EmptyIntIterator()

  /**
   * Get the singular instance of this class
   * @return the singular instance
   */
  def getInstance(): EmptyIntIterator = THE_INSTANCE
}

/**
 * An iterator over a zero-length sequence of integers
 */
class EmptyIntIterator private () extends IntIterator {

  /**
   * Test whether there are any more integers in the sequence
   *
   * @return true if there are more integers to come
   */
  def hasNext(): Boolean = false

  /**
   * Return the next integer in the sequence. The result is undefined unless hasNext() has been called
   * and has returned true.
   *
   * @return the next integer in the sequence
   */
  def next(): Int = 0
}

package client.net.sf.saxon.ce.om

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A value that exists in memory and that can be directly addressed
 */
trait GroundedValue extends Sequence {

  /**
   * Get the n'th item in the value, counting from 0
   * @param n the index of the required item, with 0 representing the first item in the sequence
   * @return the n'th item if it exists, or null otherwise
   */
  def itemAt(n: Int): Item
}

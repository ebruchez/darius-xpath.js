package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import SingletonIterator._
//remove if not needed
import scala.collection.JavaConversions._

object SingletonIterator {

  /**
   * Factory method.
   * @param item the item to iterate over
   * @return a SingletonIterator over the supplied item, or an EmptyIterator
   * if the supplied item is null.
   */
  def makeIterator(item: Item): UnfailingIterator = {
    if (item == null) {
      EmptyIterator.getInstance
    } else {
      new SingletonIterator(item)
    }
  }
}

/**
 * SingletonIterator: an iterator over a sequence of zero or one values
 */
class SingletonIterator private (value: Item) extends UnfailingIterator with GroundedIterator {

  private var item: Item = value

  private var position: Int = 0

  def next(): Item = {
    if (position == 0) {
      position = 1
      item
    } else if (position == 1) {
      position = -1
      null
    } else {
      null
    }
  }

  def getLastPosition(): Int = 1

  def getAnother(): UnfailingIterator = new SingletonIterator(item)

  def getValue(): Item = item

  /**
   * Return a Value containing all the items in the sequence returned by this
   * SequenceIterator
   *
   * @return the corresponding Value. If the value is a closure or a function call package, it will be
   * evaluated and expanded.
   */
  def materialize(): Sequence = item
}

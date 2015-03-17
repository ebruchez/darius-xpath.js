package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.expr.LastPositionFinder
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.value.EmptySequence
import client.net.sf.saxon.ce.value.SequenceExtent
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Class ListIterator, iterates over a sequence of items held in a Java List
 */
class ListIterator(var list: List[_ <: Item]) extends UnfailingIterator with LastPositionFinder with GroundedIterator {

  var index: Int = 0

  var length: Int = list.size

  /**
   * Create a ListIterator over the leading part of a given List
   *
   * @param list   the list: all objects in the list must be instances of {@link Item}
   * @param length the number of items to be included
   */
  def this(list: List[_], length: Int) {
    this()
    index = 0
    this.list = list
    this.length = length
  }

  def next(): Item = {
    if (index >= length) {
      index = -1
      length = -1
      return null
    }
    list.get(index += 1)
  }

  def getLastPosition(): Int = length

  def getAnother(): UnfailingIterator = new ListIterator(list)

  /**
   * Return a Sequence containing all the items in the sequence returned by this
   * SequenceIterator
   *
   * @return the corresponding Sequence
   */
  def materialize(): Sequence = {
    if (length == 0) {
      EmptySequence.getInstance
    } else if (length == 1) {
      list.get(0)
    } else {
      new SequenceExtent(list)
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.`type`.{AnyItemType, ItemType, Type}
import client.net.sf.saxon.ce.expr.LastPositionFinder
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.{ArrayList, LinkedList, List}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.{ArrayIterator, GroundedIterator, UnfailingIterator}
import client.net.sf.saxon.ce.tree.util.FastStringBuffer

import scala.util.control.Breaks

object SequenceExtent {

  /**
   * Factory method to make a Value holding the contents of any SequenceIterator
   * @param iter a Sequence iterator that will be consumed to deliver the items in the sequence
   * @return a ValueRepresentation holding the items delivered by the SequenceIterator. If the
   * sequence is empty the result will be an instance of [[EmptySequence]]. If it is of length
   * one, the result will be an [[Item]]. In all other cases, it will be an instance of
   * [[SequenceExtent]].
   * @throws XPathException if a dynamic error occurs while evaluating the iterator
   */
  def makeSequenceExtent(iter: SequenceIterator): Sequence = {
    if (iter.isInstanceOf[GroundedIterator]) {
      val value = iter.asInstanceOf[GroundedIterator].materialize()
      if (value != null) {
        return value
      }
    }
    val extent = SequenceExtent(iter)
    val len = extent.getLength
    if (len == 0) {
      EmptySequence.getInstance
    } else if (len == 1) {
      extent.itemAt(0)
    } else {
      extent
    }
  }

  /**
   * Factory method to make a Value holding the supplied items in reverse order
   * @param iter iterator over a List containing the items in the sequence
   * @return a ValueRepresentation holding the items in the list, in reverse
   * order of the supplied iterator
   * @throws XPathException if an error occurs evaluating the sequence
   */
  def makeReversed(iter: SequenceIterator): SequenceExtent[Item] = {
    val list = new LinkedList[Item]()
    import Breaks._
    breakable {
      while (true) {
        val item = iter.next()
        if (item == null) {
          break()
        }
        list.addFirst(item)
      }
    }
    SequenceExtent(list)
  }

  /**
   * Factory method to make a Value holding the contents of any List of items
   * @param input a List containing the items in the sequence
   * @return a ValueRepresentation holding the items in the list. If the
   * sequence is empty the result will be an instance of [[EmptySequence]]. If it is of length
   * one, the result will be an [[Item]]. In all other cases, it will be an instance of
   * [[SequenceExtent]].
   */
  def makeSequenceExtent(input: List[Item]): Sequence = {
    val len = input.size
    if (len == 0) {
      EmptySequence.getInstance
    } else if (len == 1) {
      input.get(0)
    } else {
      SequenceExtent(input)
    }
  }

  //ORBEON: There is makeSequenceExtent above, and the following, and both take a SequenceIterator! Confusing!
  /**
   * Construct a sequence containing all the items in a SequenceIterator.
   *
   * @throws client.net.sf.saxon.ce.trans.XPathException if reading the items using the
   *     SequenceIterator raises an error
   * @param iter The supplied sequence of items. This must be positioned at
   *     the start, so that hasNext() returns true if there are any nodes in
   *      the node-set, and next() returns the first node.
   */
  def apply(iter: SequenceIterator): SequenceExtent[Item] = {
    var allocated = -1
    if (iter.isInstanceOf[LastPositionFinder]) {
      allocated = iter.asInstanceOf[LastPositionFinder].getLastPosition
    }
    import Breaks._
    val value =
      if (allocated == -1) {
        val list = new ArrayList[Item](20)
        breakable {
          while (true) {
            val it = iter.next()
            if (it == null) {
              break()
            }
            list.add(it)
          }
        }
        val array = new Array[Item](list.size)
        list.toArray(array)
      } else {
        val result = new Array[Item](allocated)
        var i = 0
        breakable {
          while (true) {
            val it = iter.next()
            if (it == null) {
              break()
            }
            result(i) = it
            i += 1
          }
        }
        result
      }
    
    new SequenceExtent(value)
  }

  def apply[T  <: Item](list: List[T]): SequenceExtent[Item] =
    new SequenceExtent(list.toArray(new Array[Item](list.size)))
}

/**
 * A sequence value implemented extensionally. That is, this class represents a sequence
 * by allocating memory to each item in the sequence.
 */
class SequenceExtent[T <: Item](val value: Array[T]) extends Sequence {

  private var itemType: ItemType = null

  /**
   * Simplify this SequenceExtent
   * @return a Value holding the items delivered by the SequenceIterator. If the
   * sequence is empty the result will be an instance of [[EmptySequence]]. If it is of length
   * one, the result will be an [[AtomicValue]] or a [[client.net.sf.saxon.ce.om.NodeInfo]].
   * In all other cases, the [[SequenceExtent]] will be returned unchanged.
   */
  def simplify(): Sequence = {
    val n = getLength
    if (n == 0) {
      EmptySequence.getInstance
    } else if (n == 1) {
      itemAt(0)
    } else {
      this
    }
  }

  /**
   * Get the number of items in the sequence
   *
   * @return the number of items in the sequence
   */
  def getLength(): Int = value.length

  /**
   * Get the (lowest common) item type
   *
   * @return integer identifying an item type to which all the items in this
   *      sequence conform
   */
  def getItemType(): ItemType = {
    if (itemType != null) {
      return itemType
    }
    itemType = Type.getItemType(value(0))
    for (i <- 1 until value.length) {
      if (itemType == AnyItemType.getInstance) {
        return itemType
      }
      itemType = Type.getCommonSuperType(itemType, Type.getItemType(value(i)))
    }
    itemType
  }

  /**
   * Get the n'th item in the sequence (starting with 0 as the first item)
   *
   * @param n the position of the required item
   * @return the n'th item in the sequence, zero-based, or null if n is out of range
   */
  def itemAt(n: Int): Item = {
    if (n < 0 || n >= getLength) {
      null
    } else {
      value(n)
    }
  }

  /**
   * Swap two items (needed to support sorting)
   *
   * @param a the position of the first item to be swapped
   * @param b the position of the second item to be swapped
   */
  def swap(a: Int, b: Int): Unit = {
    val temp = value(a)
    value(a) = value(b)
    value(b) = temp
  }

  /**
   * Return an iterator over this sequence.
   *
   * @return the required SequenceIterator, positioned at the start of the
   *     sequence
   */
  def iterate(): UnfailingIterator = new ArrayIterator(value)

  override def toString(): String = {
    val fsb = new FastStringBuffer(FastStringBuffer.SMALL)
    fsb.append('(')
    for (i <- 0 until value.length) {
      fsb.append(value(i).toString)
      if (i != value.length - 1) {
        fsb.append(", ")
      }
    }
    fsb.append(')')
    fsb.toString
  }
}

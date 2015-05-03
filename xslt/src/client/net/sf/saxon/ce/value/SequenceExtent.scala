// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import org.orbeon.darius.xpath.expr.LastPositionFinder
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.Sequence
import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.ArrayIterator
import org.orbeon.darius.xpath.tree.iter.GroundedIterator
import org.orbeon.darius.xpath.tree.iter.UnfailingIterator
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.`type`.AnyItemType
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.`type`.Type
import java.util.ArrayList
import java.util.LinkedList
import java.util.List
import SequenceExtent._
//remove if not needed
import scala.collection.JavaConversions._

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
    val extent = new SequenceExtent(iter)
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
  def makeReversed(iter: SequenceIterator): SequenceExtent = {
    val list = new LinkedList[Item]()
    while (true) {
      val item = iter.next()
      if (item == null) {
        //break
      }
      list.addFirst(item)
    }
    new SequenceExtent(list)
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
      new SequenceExtent(input)
    }
  }
}

/**
 * A sequence value implemented extensionally. That is, this class represents a sequence
 * by allocating memory to each item in the sequence.
 */
class SequenceExtent(@transient var value: Array[Item]) extends Sequence {

  private var itemType: ItemType = null

  /**
   * Construct a SequenceExtent from a List. The members of the list must all
   * be Items
   *
   * @param list the list of items to be included in the sequence
   */
  def this(list: List[_ <: Item]) {
    this()
    val array = Array.ofDim[Item](list.size)
    value = list.toArray(array)
  }

  /**
   * Construct a sequence containing all the items in a SequenceIterator.
   *
   * @exception client.net.sf.saxon.ce.trans.XPathException if reading the items using the
   *     SequenceIterator raises an error
   * @param iter The supplied sequence of items. This must be positioned at
   *     the start, so that hasNext() returns true if there are any nodes in
   *      the node-set, and next() returns the first node.
   */
  def this(iter: SequenceIterator) {
    this()
    var allocated = -1
    if (iter.isInstanceOf[LastPositionFinder]) {
      allocated = iter.asInstanceOf[LastPositionFinder].getLastPosition
    }
    if (allocated == -1) {
      val list = new ArrayList[Item](20)
      while (true) {
        val it = iter.next()
        if (it == null) {
          //break
        }
        list.add(it)
      }
      val array = Array.ofDim[Item](list.size)
      value = list.toArray(array)
    } else {
      value = Array.ofDim[Item](allocated)
      val i = 0
      while (true) {
        val it = iter.next()
        if (it == null) {
          //break
        }
        value(i += 1) = it
      }
    }
  }

  /**
   * Simplify this SequenceExtent
   * @return a Value holding the items delivered by the SequenceIterator. If the
   * sequence is empty the result will be an instance of [[EmptySequence]]. If it is of length
   * one, the result will be an [[AtomicValue]] or a [[org.orbeon.darius.xpath.om.NodeInfo]].
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
  def getLength: Int = value.length

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
    for (i ← 1 until value.length) {
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

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.SMALL)
    fsb.append('(')
    for (i ← 0 until value.length) {
      fsb.append(value(i).toString)
      if (i != value.length - 1) {
        fsb.append(", ")
      }
    }
    fsb.append(')')
    fsb.toString
  }
}

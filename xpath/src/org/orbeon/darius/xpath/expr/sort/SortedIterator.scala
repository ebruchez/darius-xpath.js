// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.expr.{LastPositionFinder, XPathContext}
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.FocusIterator
import org.orbeon.darius.xpath.value.AtomicValue

/**
 * Class to do a sorted iteration
 */
class SortedIterator private () extends SequenceIterator with LastPositionFinder with Sortable {

  protected var base: FocusIterator = _

  protected var sortKeyEvaluator: SortKeyEvaluator = _

  protected var comparators: Array[AtomicComparer] = _

  protected var recordSize: Int = _

  protected var nodeKeys: Array[Any] = _

  protected var count: Int = -1

  protected var position: Int = 0

  protected var context: XPathContext = _

  /**
   * Create a sorted iterator
   * @param context the dynamic XPath evaluation context
   * @param base an iterator over the sequence to be sorted
   * @param sortKeyEvaluator an object that allows the n'th sort key for a given item to be evaluated
   * @param comparators an array of AtomicComparers, one for each sort key, for comparing sort key values
   */
  def this(context: XPathContext, 
      base: SequenceIterator, 
      sortKeyEvaluator: SortKeyEvaluator, 
      comparators: Array[AtomicComparer]) {
    this()
    this.context = context.newMinorContext()
    this.base = this.context.setCurrentIterator(base)
    this.sortKeyEvaluator = sortKeyEvaluator
    this.comparators = comparators
    recordSize = comparators.length + 2
  }

  /**
   * Get the next item, in sorted order
   */
  def next(): Item = {
    if (position < 0) {
      return null
    }
    if (count < 0) {
      doSort()
    }
    if (position < count) {
      position += 1
      nodeKeys(position * recordSize).asInstanceOf[Item]
    } else {
      position = -1
      null
    }
  }

  def current(): Item = {
    if (position < 1) {
      return null
    }
    nodeKeys((position - 1) * recordSize).asInstanceOf[Item]
  }

  def getLastPosition: Int = {
    if (count < 0) {
      doSort()
    }
    count
  }

  def getAnother: SequenceIterator = {
    if (count < 0) {
      doSort()
    }
    val s = new SortedIterator()
    s.base = base.getAnother.asInstanceOf[FocusIterator]
    s.sortKeyEvaluator = sortKeyEvaluator
    s.comparators = comparators
    s.recordSize = recordSize
    s.nodeKeys = nodeKeys
    s.count = count
    s.context = context
    s.position = 0
    s
  }

  /**
   * Create an array holding the items to be sorted and the values of their sort keys
   * @throws XPathException
   */
  protected def buildArray(): Unit = {
    var allocated = -1
    if (base.getUnderlyingIterator.isInstanceOf[LastPositionFinder]) {
      allocated = base.last()
    }
    if (allocated == -1) {
      allocated = 100
    }
    nodeKeys = new Array[Any](allocated * recordSize)
    count = 0
    allocated = populateArray(allocated)
    if (allocated * 2 < count || (allocated - count) > 2000) {
      val nk2 = new Array[Any](count * recordSize)
      System.arraycopy(nodeKeys, 0, nk2, 0, count * recordSize)
      nodeKeys = nk2
    }
  }

  protected def populateArray(_allocated: Int): Int = {
    var allocated = _allocated
    while (true) {
      val item = base.next()
      if (item == null) {
        return allocated
      }
      if (count == allocated) {
        allocated *= 2
        val nk2 = new Array[Any](allocated * recordSize)
        System.arraycopy(nodeKeys, 0, nk2, 0, count * recordSize)
        nodeKeys = nk2
      }
      val k = count * recordSize
      nodeKeys(k) = item
      for (n ← comparators.indices) {
        nodeKeys(k + n + 1) = sortKeyEvaluator.evaluateSortKey(n, context)
      }
      nodeKeys(k + comparators.length + 1) = count
      count += 1
    }
    throw new IllegalStateException
  }

  private def doSort(): Unit = {
    buildArray()
    if (count < 2) return
    try {
      GenericSorter.quickSort(0, count, this)
    } catch {
      case e: ClassCastException ⇒
        val err = new XPathException("Non-comparable types found while sorting: " + e.getMessage)
        err.setErrorCode("XTDE1030")
        throw err
    }
  }

  /**
   * Compare two items in sorted sequence
   * (needed to implement the Sortable interface)
   * @return <0 if obj[a]<obj[b], 0 if obj[a]=obj[b], >0 if obj[a]>obj[b]
   */
  def compare(a: Int, b: Int): Int = {
    val a1 = a * recordSize + 1
    val b1 = b * recordSize + 1
    for (i ← comparators.indices) {
      val comp = comparators(i).compareAtomicValues(nodeKeys(a1 + i).asInstanceOf[AtomicValue], nodeKeys(b1 + i).asInstanceOf[AtomicValue])
      if (comp != 0) {
        return comp
      }
    }
    nodeKeys(a1 + comparators.length).asInstanceOf[java.lang.Integer]
      .intValue() - 
      nodeKeys(b1 + comparators.length).asInstanceOf[java.lang.Integer]
      .intValue()
  }

  /**
   * Swap two items (needed to implement the Sortable interface)
   */
  def swap(a: Int, b: Int): Unit = {
    val a1 = a * recordSize
    val b1 = b * recordSize
    for (i ← 0 until recordSize) {
      val temp = nodeKeys(a1 + i)
      nodeKeys(a1 + i) = nodeKeys(b1 + i)
      nodeKeys(b1 + i) = temp
    }
  }
}

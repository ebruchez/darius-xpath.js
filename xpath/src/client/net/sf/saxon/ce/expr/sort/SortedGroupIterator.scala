package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A SortedGroupIterator is a modified SortedIterator. It sorts a sequence of groups,
 * and is itself a GroupIterator. The modifications retain extra information about
 * the items being sorted. The items are each the leading item of a group, and as well
 * as the item itself, the iterator preserves information about the group: specifically,
 * an iterator over the items in the group, and the value of the grouping key (if any).
 */
class SortedGroupIterator(context: XPathContext, 
    base: GroupIterator, 
    sortKeyEvaluator: SortKeyEvaluator, 
    comparators: Array[AtomicComparer]) extends SortedIterator(context, base, sortKeyEvaluator, comparators) with GroupIterator {

  recordSize += 2

  /**
   * Override the method that populates the array of values and sort keys.
   * @throws XPathException
   */
  protected def populateArray(allocated: Int): Int = {
    val c2 = context.newContext()
    c2.setCurrentGroupIterator(base.getUnderlyingIterator.asInstanceOf[GroupIterator])
    while (true) {
      val item = base.next()
      if (item == null) {
        //break
      }
      if (count == allocated) {
        allocated *= 2
        val nk2 = Array.ofDim[Any](allocated * recordSize)
        System.arraycopy(nodeKeys, 0, nk2, 0, count * recordSize)
        nodeKeys = nk2
      }
      val k = count * recordSize
      nodeKeys(k) = item
      for (n <- 0 until comparators.length) {
        nodeKeys(k + n + 1) = sortKeyEvaluator.evaluateSortKey(n, c2)
      }
      nodeKeys(k + comparators.length + 1) = count
      val gi = base.getUnderlyingIterator.asInstanceOf[GroupIterator]
      nodeKeys(k + comparators.length + 2) = gi.getCurrentGroupingKey
      nodeKeys(k + comparators.length + 3) = gi.iterateCurrentGroup()
      count += 1
    }
    allocated
  }

  def getCurrentGroupingKey(): AtomicValue = {
    nodeKeys((position - 1) * recordSize + comparators.length + 2).asInstanceOf[AtomicValue]
  }

  def iterateCurrentGroup(): SequenceIterator = {
    val iter = nodeKeys((position - 1) * recordSize + comparators.length + 3).asInstanceOf[SequenceIterator]
    iter.getAnother
  }
}

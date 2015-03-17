package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceExtent
//remove if not needed
import scala.collection.JavaConversions._

/**
 * DocumentOrderIterator takes as input an iteration of nodes in any order, and
 * returns as output an iteration of the same nodes in document order, eliminating
 * any duplicates.
 */
class DocumentOrderIterator(base: SequenceIterator, var comparer: NodeOrderComparer)
    extends SequenceIterator with Sortable {

  private var iterator: SequenceIterator = sequence.iterate()

  private var sequence: SequenceExtent = new SequenceExtent(base)

  private var current: NodeInfo = null

  if (sequence.getLength > 1) {
    GenericSorter.quickSort(0, sequence.getLength, this)
  }

  /**
   * Private constructor used only by getAnother()
   */
  private def this() {
  }

  /**
   * Compare two nodes in document sequence
   * (needed to implement the Sortable interface)
   */
  def compare(a: Int, b: Int): Int = {
    comparer.compare(sequence.itemAt(a).asInstanceOf[NodeInfo], sequence.itemAt(b).asInstanceOf[NodeInfo])
  }

  /**
   * Swap two nodes (needed to implement the Sortable interface)
   */
  def swap(a: Int, b: Int) {
    sequence.swap(a, b)
  }

  def next(): Item = {
    while (true) {
      val next = iterator.next().asInstanceOf[NodeInfo]
      if (next == null) {
        current = null
        return null
      }
      if (current != null && next.isSameNodeInfo(current)) {
        //continue
      } else {
        current = next
        current
      }
    }
  }

  def getAnother(): SequenceIterator = {
    val another = new DocumentOrderIterator()
    another.iterator = iterator.getAnother
    another
  }
}

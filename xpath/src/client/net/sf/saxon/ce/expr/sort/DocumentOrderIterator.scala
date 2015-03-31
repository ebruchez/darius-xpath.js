// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.{Item, NodeInfo, SequenceIterator}
import client.net.sf.saxon.ce.value.SequenceExtent

/**
 * DocumentOrderIterator takes as input an iteration of nodes in any order, and
 * returns as output an iteration of the same nodes in document order, eliminating
 * any duplicates.
 */
class DocumentOrderIterator(base: SequenceIterator, var comparer: NodeOrderComparer)
    extends SequenceIterator with Sortable {

  private val sequence: SequenceExtent[Item] = if (base ne null) SequenceExtent(base) else null
  private var iterator: SequenceIterator = sequence.iterate()

  private var current: NodeInfo = null

  if (sequence.getLength > 1) {
    GenericSorter.quickSort(0, sequence.getLength, this)
  }

  /**
   * Private constructor used only by getAnother()
   */
  private def this() =
    this(null, null)

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
  def swap(a: Int, b: Int): Unit = {
    sequence.swap(a, b)
  }

  def next(): Item = {
    while (true) {
      val next = iterator.next().asInstanceOf[NodeInfo]
      if (next == null) {
        current = null
        return null
      }
      if (! (current != null && next.isSameNodeInfo(current))) {//ORBEON: was continue, negated condition
        current = next
        return current
      }
    }
    throw new IllegalStateException
  }

  def getAnother: SequenceIterator = {
    val another = new DocumentOrderIterator()
    another.iterator = iterator.getAnother
    another
  }
}

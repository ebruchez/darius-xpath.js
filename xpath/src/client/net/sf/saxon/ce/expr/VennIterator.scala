package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.expr.sort.NodeOrderComparer
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An iterator representing a nodeset that is a union, intersection, or difference of two other NodeSets.
 * The input iterators are both assumed to be in document order.
 */
class VennIterator(p1: SequenceIterator, 
    p2: SequenceIterator, 
    var comparer: NodeOrderComparer, 
    var operator: Int) extends SequenceIterator {

  private var e1: SequenceIterator = p1

  private var e2: SequenceIterator = p2

  private var nextNode1: NodeInfo = next(e1)

  private var nextNode2: NodeInfo = next(e2)

  private var current: NodeInfo = null

  /**
   * Get the next item from one of the input sequences,
   * checking that it is a node.
   * @param iter the sequence from which a node is to be read
   * @return the node that was read
   * @throws XPathException if the next node cannot be read
   */
  private def next(iter: SequenceIterator): NodeInfo = iter.next().asInstanceOf[NodeInfo]

  def next(): Item = operator match {
    case Token.UNION => 
      if (nextNode1 != null && nextNode2 != null) {
        val c = comparer.compare(nextNode1, nextNode2)
        if (c < 0) {
          deliver1()
        } else if (c > 0) {
          deliver2()
        } else {
          deliverCommon()
        }
      }
      if (nextNode1 != null) {
        deliver1()
      }
      if (nextNode2 != null) {
        deliver2()
      }
      deliverEndOfSequence()

    case Token.INTERSECT => 
      if (nextNode1 == null || nextNode2 == null) {
        deliverEndOfSequence()
      }
      while (nextNode1 != null && nextNode2 != null) {
        val c = comparer.compare(nextNode1, nextNode2)
        if (c < 0) {
          nextNode1 = next(e1)
        } else if (c > 0) {
          nextNode2 = next(e2)
        } else {
          deliverCommon()
        }
      }
      deliverEndOfSequence()

    case Token.EXCEPT => while (true) {
      if (nextNode1 == null) {
        deliverEndOfSequence()
      }
      if (nextNode2 == null) {
        return deliver1()
      }
      val c = comparer.compare(nextNode1, nextNode2)
      if (c < 0) {
        deliver1()
      } else if (c > 0) {
        nextNode2 = next(e2)
        if (nextNode2 == null) {
          deliver1()
        }
      } else {
        nextNode2 = next(e2)
        nextNode1 = next(e1)
      }
    }
    case _ => null
  }

  /**
   * Deliver the next node from the first node-set, advancing the iterator to
   * look-ahead for the next item, and setting the current and position variables.
   * @return the next node from the first node-set
   * @throws XPathException on failure to read the next node
   */
  private def deliver1(): NodeInfo = {
    current = nextNode1
    nextNode1 = next(e1)
    current
  }

  /**
   * Deliver the next node from the second node-set, advancing the iterator to
   * look-ahead for the next item, and setting the current and position variables.
   * @return the next node from the first node-set
   * @throws XPathException on failure to read the next node
   */
  private def deliver2(): NodeInfo = {
    current = nextNode2
    nextNode2 = next(e2)
    current
  }

  /**
   * Deliver the next node when it is the same in both node-sets, advancing each iterator to
   * look-ahead for the next item, and setting the current and position variables.
   * @return the next node from the first node-set, which is the same as the next node from the second node-set
   * @throws XPathException on failure to read the next node
   */
  private def deliverCommon(): NodeInfo = {
    current = nextNode1
    nextNode1 = next(e2)
    nextNode2 = next(e2)
    current
  }

  /**
   * Deliver the end-of-sequence. Set current to null and position to -1.
   * @return null, always
   */
  private def deliverEndOfSequence(): NodeInfo = {
    current = null
    null
  }

  def getAnother(): SequenceIterator = {
    new VennIterator(e1.getAnother, e2.getAnother, comparer, operator)
  }
}

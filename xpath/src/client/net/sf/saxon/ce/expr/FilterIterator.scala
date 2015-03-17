package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.FocusIterator
import client.net.sf.saxon.ce.value.BooleanValue
import client.net.sf.saxon.ce.value.NumericValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A FilterIterator filters an input sequence using a filter expression. Note that a FilterIterator
 * is not used where the filter is a constant number (PositionFilter is used for this purpose instead),
 * so this class does no optimizations for numeric predicates.
 */
class FilterIterator(base: SequenceIterator, protected var filter: Expression, context: XPathContext)
    extends SequenceIterator {

  protected var base: FocusIterator = filterContext.setCurrentIterator(base)

  protected var filterContext: XPathContext = context.newMinorContext()

  /**
   * Get the next item if there is one
   */
  def next(): Item = getNextMatchingItem

  /**
   * Get the next item in the base sequence that matches the filter predicate
   * if there is such an item, or null if not.
   * @return the next item that matches the predicate
   */
  protected def getNextMatchingItem(): Item = {
    while (true) {
      val next = base.next()
      if (next == null) {
        return null
      }
      if (matches()) {
        next
      }
    }
  }

  /**
   * Determine whether the context item matches the filter predicate
   * @return true if the context item matches
   */
  protected def matches(): Boolean = {
    val iterator = filter.iterate(filterContext)
    val first = iterator.next()
    if (first == null) {
      return false
    }
    if (first.isInstanceOf[NodeInfo]) {
      true
    } else {
      if (iterator.next() != null) {
        ExpressionTool.ebvError("sequence of two or more atomic values")
      }
      if (first.isInstanceOf[BooleanValue]) {
        first.asInstanceOf[BooleanValue].getBooleanValue
      } else if (first.isInstanceOf[StringValue]) {
        (first.getStringValue.length != 0)
      } else if (first.isInstanceOf[NumericValue]) {
        first.asInstanceOf[NumericValue].compareTo(base.position()) == 
          0
      } else {
        ExpressionTool.ebvError("sequence starting with an atomic value other than a boolean, number, or string")
        false
      }
    }
  }

  /**
   * Get another iterator to return the same nodes
   */
  def getAnother(): SequenceIterator = {
    new FilterIterator(base.getAnother, filter, filterContext)
  }
}

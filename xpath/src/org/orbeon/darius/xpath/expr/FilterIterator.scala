// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.om.{Item, NodeInfo, SequenceIterator}
import org.orbeon.darius.xpath.tree.iter.FocusIterator
import org.orbeon.darius.xpath.value.{BooleanValue, NumericValue, StringValue}

/**
 * A FilterIterator filters an input sequence using a filter expression. Note that a FilterIterator
 * is not used where the filter is a constant number (PositionFilter is used for this purpose instead),
 * so this class does no optimizations for numeric predicates.
 */
class FilterIterator(_base: SequenceIterator, protected var filter: Expression, context: XPathContext)
    extends SequenceIterator {

  protected var filterContext: XPathContext = context.newMinorContext()
  protected var base: FocusIterator = filterContext.setCurrentIterator(_base)

  /**
   * Get the next item if there is one
   */
  def next(): Item = getNextMatchingItem

  /**
   * Get the next item in the base sequence that matches the filter predicate
   * if there is such an item, or null if not.
   * @return the next item that matches the predicate
   */
  protected def getNextMatchingItem: Item = {
    while (true) {
      val next = base.next()
      if (next == null) {
        return null
      }
      if (matches()) {
        return next
      }
    }
    throw new IllegalStateException
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
      first match {
        case value: BooleanValue ⇒
          value.getBooleanValue
        case _: StringValue ⇒
          first.getStringValue.length != 0
        case value: NumericValue ⇒
          value.compareTo(base.position()) == 0
        case _ ⇒
          ExpressionTool.ebvError("sequence starting with an atomic value other than a boolean, number, or string")
      }
    }
  }

  /**
   * Get another iterator to return the same nodes
   */
  def getAnother: SequenceIterator = {
    new FilterIterator(base.getAnother, filter, filterContext)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.expr.ItemMappingIterator._
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.trans.XPathException

object ItemMappingIterator {

  /**
   * The mapping function can throw an EarlyExitException to indicate that no more items will be
   * returned; processing of the input sequence can cease at this point.
   */
  class EarlyExitException extends XPathException
}

/**
 * ItemMappingIterator applies a mapping function to each item in a sequence.
 * The mapping function either returns a single item, or null (representing an
 * empty sequence).
 * <p/>
 * This is a specialization of the more general MappingIterator class, for use
 * in cases where a single input item never maps to a sequence of more than one
 * output item.
 */
class ItemMappingIterator(var base: SequenceIterator, var action: ItemMappingFunction, private var oneToOne: Boolean)
    extends SequenceIterator with LastPositionFinder {

  /**
   * Construct an ItemMappingIterator that will apply a specified ItemMappingFunction to
   * each Item returned by the base iterator.
   *
   * @param base   the base iterator
   * @param action the mapping function to be applied
   */
  def this(base: SequenceIterator, action: ItemMappingFunction) =
    this(base, action, oneToOne = false)

  protected def getBaseIterator: SequenceIterator = base

  protected def getMappingFunction: ItemMappingFunction = action

  def next(): Item = {
    while (true) {
      val nextSource = base.next()
      if (nextSource == null) {
        return null
      }
      try {
        val curr = action.mapItem(nextSource)
        if (curr != null) {
          return curr
        }
      } catch {
        case e: EarlyExitException ⇒
          return null
      }
    }
    throw new IllegalStateException
  }

  def getAnother: SequenceIterator = {
    val newBase = base.getAnother
    val newAction = action match {
      case function: StatefulMappingFunction ⇒ function.getAnother(newBase).asInstanceOf[ItemMappingFunction]
      case _ ⇒ action
    }
    new ItemMappingIterator(newBase, newAction, oneToOne)
  }

  def getLastPosition: Int = {
    base match {
      case finder: LastPositionFinder if oneToOne ⇒ finder.getLastPosition
      case _ ⇒ -1
    }
  }
}

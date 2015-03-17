// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.{Item, SequenceIterator}

/**
 * ContextMappingIterator merges a sequence of sequences into a single flat
 * sequence. It takes as inputs an iteration, and a mapping function to be
 * applied to each Item returned by that iteration. The mapping function itself
 * returns another iteration. The result is an iteration of the concatenation of all
 * the iterations returned by the mapping function.<p>
 *
 * This is a specialization of the MappingIterator class: it differs in that it
 * sets each item being processed as the context item
 */
class ContextMappingIterator(var action: ContextMappingFunction, var context: XPathContext)
    extends SequenceIterator {

  private val base: SequenceIterator = context.getCurrentIterator

  private var stepIterator: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    while (true) {
      if (stepIterator != null) {
        nextItem = stepIterator.next()
        if (nextItem != null) {
          return nextItem
        } else {
          stepIterator = null
        }
      }
      if (base.next() != null) {
        stepIterator = action.map(context)
        nextItem = stepIterator.next()
        if (nextItem == null) {
          stepIterator = null
        } else {
          return nextItem
        }
      } else {
        stepIterator = null
        return null
      }
    }
    throw new IllegalStateException
  }

  def getAnother(): SequenceIterator = {
    val newBase = base.getAnother
    val c2 = context.newMinorContext()
    c2.setCurrentIterator(newBase)
    new ContextMappingIterator(action, c2)
  }
}

package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

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

  private var base: SequenceIterator = context.getCurrentIterator

  private var stepIterator: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    while (true) {
      if (stepIterator != null) {
        nextItem = stepIterator.next()
        if (nextItem != null) {
          //break
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
          //break
        }
      } else {
        stepIterator = null
        return null
      }
    }
    nextItem
  }

  def getAnother(): SequenceIterator = {
    val newBase = base.getAnother
    val c2 = context.newMinorContext()
    c2.setCurrentIterator(newBase)
    new ContextMappingIterator(action, c2)
  }
}

package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * MappingIterator merges a sequence of sequences into a single flat
 * sequence. It takes as inputs an iteration, and a mapping function to be
 * applied to each Item returned by that iteration. The mapping function itself
 * returns another iteration. The result is an iteration of the concatenation of all
 * the iterations returned by the mapping function.<p>
 *
 * This is a powerful class. It is used, with different mapping functions,
 * in a great variety of ways. It underpins the way that "for" expressions and
 * path expressions are evaluated, as well as sequence expressions. It is also
 * used in the implementation of the document(), key(), and id() functions.
 */
class MappingIterator(var base: SequenceIterator, var action: MappingFunction)
    extends SequenceIterator {

  private var results: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    while (true) {
      if (results != null) {
        nextItem = results.next()
        if (nextItem != null) {
          //break
        } else {
          results = null
        }
      }
      val nextSource = base.next()
      if (nextSource != null) {
        val obj = action.map(nextSource)
        if (obj != null) {
          results = obj
          nextItem = results.next()
          if (nextItem == null) {
            results = null
          } else {
            //break
          }
        }
      } else {
        results = null
        return null
      }
    }
    nextItem
  }

  def getAnother(): SequenceIterator = {
    val newBase = base.getAnother
    val newAction = if (action.isInstanceOf[StatefulMappingFunction]) action.asInstanceOf[StatefulMappingFunction].getAnother(null).asInstanceOf[MappingFunction] else action
    new MappingIterator(newBase, newAction)
  }
}

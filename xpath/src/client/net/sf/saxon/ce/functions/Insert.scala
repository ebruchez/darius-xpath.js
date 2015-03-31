// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.Insert._
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.value.{AtomicValue, NumericValue}

object Insert {

  class InsertIterator(var base: SequenceIterator, var insert: SequenceIterator, _insertPosition: Int)
      extends SequenceIterator {

    private val insertPosition: Int = if (_insertPosition < 1) 1 else _insertPosition

    private var position: Int = 0

    private var current: Item = null

    private var inserting: Boolean = insertPosition == 1

    def next(): Item = {
      var nextItem: Item = null
      if (inserting) {
        nextItem = insert.next()
        if (nextItem == null) {
          inserting = false
          nextItem = base.next()
        }
      } else {
        if (position == insertPosition - 1) {
          nextItem = insert.next()
          if (nextItem == null) {
            nextItem = base.next()
          } else {
            inserting = true
          }
        } else {
          nextItem = base.next()
          if (nextItem == null && position < insertPosition - 1) {
            inserting = true
            nextItem = insert.next()
          }
        }
      }
      if (nextItem == null) {
        current = null
        position = -1
        null
      } else {
        current = nextItem
        position += 1
        current
      }
    }

    def getAnother(): SequenceIterator = {
      new InsertIterator(base.getAnother, insert.getAnother, insertPosition)
    }
  }
}

/**
 * The XPath 2.0 insert-before() function
 */
class Insert extends SystemFunction {

  def newInstance(): Insert = new Insert()

  /**
   * Evaluate the function to return an iteration of selected nodes.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val seq = argument(0).iterate(context)
    val n0 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    val n = n0.asInstanceOf[NumericValue]
    val pos = n.intValue().toInt
    val ins = argument(2).iterate(context)
    new InsertIterator(seq, ins, pos)
  }
}

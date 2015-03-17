package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.NumericValue
import Insert._
//remove if not needed
import scala.collection.JavaConversions._

object Insert {

  class InsertIterator(var base: SequenceIterator, var insert: SequenceIterator, insertPosition: Int)
      extends SequenceIterator {

    private var insertPosition: Int = (if (insertPosition < 1) 1 else insertPosition)

    private var position: Int = 0

    private var current: Item = null

    private var inserting: Boolean = (insertPosition == 1)

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
  def iterate(context: XPathContext): SequenceIterator = {
    val seq = argument(0).iterate(context)
    val n0 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    val n = n0.asInstanceOf[NumericValue]
    val pos = n.intValue().toInt
    val ins = argument(2).iterate(context)
    new InsertIterator(seq, ins, pos)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.{LastPositionFinder, XPathContext}
import client.net.sf.saxon.ce.functions.Count._
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.value._

object Count {

  /**
   * Get the number of items in a sequence identified by a SequenceIterator
   * @param iter The SequenceIterator. This method moves the current position
   * of the supplied iterator; if this isn't safe, make a copy of the iterator
   * first by calling getAnother(). The supplied iterator must be positioned
   * before the first item (there must have been no call on next()).
   * @return the number of items in the underlying sequence
   * @throws client.net.sf.saxon.ce.trans.XPathException if a failure occurs reading the input sequence
   */
  def count(iter: SequenceIterator): Int = {
    var count = -1
    if (iter.isInstanceOf[LastPositionFinder]) {
      count = iter.asInstanceOf[LastPositionFinder].getLastPosition
    }
    if (count == -1) {
      count = 0
      while (iter.next() != null) {
        count += 1
      }
    }
    count
  }
}

/**
 * Implementation of the fn:count function
 */
class Count extends Aggregate {

  def newInstance(): Count = new Count()

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val iter = argument(0).iterate(context)
    new IntegerValue(count(iter))
  }
}

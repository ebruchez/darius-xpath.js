// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import SingletonIterator._
//remove if not needed
import scala.collection.JavaConversions._

object SingletonIterator {

  /**
   * Factory method.
   * @param item the item to iterate over
   * @return a SingletonIterator over the supplied item, or an EmptyIterator
   * if the supplied item is null.
   */
  def makeIterator(item: Item): UnfailingIterator = {
    if (item == null) {
      EmptyIterator.getInstance
    } else {
      new SingletonIterator(item)
    }
  }
}

/**
 * SingletonIterator: an iterator over a sequence of zero or one values
 */
class SingletonIterator private (value: Item) extends UnfailingIterator with GroundedIterator {

  private var item: Item = value

  private var position: Int = 0

  def next(): Item = {
    if (position == 0) {
      position = 1
      item
    } else if (position == 1) {
      position = -1
      null
    } else {
      null
    }
  }

  def getLastPosition(): Int = 1

  def getAnother(): UnfailingIterator = new SingletonIterator(item)

  def getValue(): Item = item

  /**
   * Return a Value containing all the items in the sequence returned by this
   * SequenceIterator
   *
   * @return the corresponding Value. If the value is a closure or a function call package, it will be
   * evaluated and expanded.
   */
  def materialize(): Sequence = item
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.`type`.ItemType
import EmptySequence._
//remove if not needed
import scala.collection.JavaConversions._

object EmptySequence {

  private var THE_INSTANCE: EmptySequence = new EmptySequence()

  /**
   * Get the implicit instance of this class
   */
  def getInstance(): EmptySequence = THE_INSTANCE
}

/**
 * An EmptySequence object represents a sequence containing no members.
 */
class EmptySequence private () extends Sequence {

  /**
   * Return an iteration over the sequence
   */
  def iterate(): UnfailingIterator = EmptyIterator.getInstance

  override def toString(): String = "()"

  /**
   * Determine the item type
   */
  def getItemType(): ItemType = EmptySequenceTest.getInstance

  /**
   * Get the length of the sequence
   * @return always 0 for an empty sequence
   */
  def getLength: Int = 0

  /**
   * Is this expression the same as another expression?
   * @throws ClassCastException if the values are not comparable
   */
  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[EmptySequence]) {
      throw new ClassCastException("Cannot compare " + other.getClass + " to empty sequence")
    }
    true
  }

  override def hashCode(): Int = 42

  /**
   * Get the effective boolean value - always false
   */
  def effectiveBooleanValue(): Boolean = false

  /**
   * Get the n'th item in the sequence (starting from 0). This is defined for all
   * Values, but its real benefits come for a sequence Value stored extensionally
   * (or for a MemoClosure, once all the values have been read)
   *
   * @param n position of the required item, counting from zero.
   * @return the n'th item in the sequence, where the first item in the sequence is
   *         numbered zero. If n is negative or >= the length of the sequence, returns null.
   */
  def itemAt(n: Int): Item = null
}

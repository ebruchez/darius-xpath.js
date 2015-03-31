// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

/**
 * An immutable integer set containing a single integer
 */
class IntSingletonSet(var value: Int) extends IntSet {

  def getMember: Int = value

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntSingletonSet is immutable")
  }

  def copy(): IntSet = this

  def mutableCopy(): IntSet = {
    val intHashSet = new IntHashSet()
    intHashSet.add(value)
    intHashSet
  }

  def size(): Int = 1

  def isEmpty(): Boolean = false

  def contains(value: Int): Boolean = this.value == value

  def remove(value: Int): Boolean = {
    throw new UnsupportedOperationException("IntSingletonSet is immutable")
  }

  def add(value: Int): Boolean = {
    throw new UnsupportedOperationException("IntSingletonSet is immutable")
  }

  def iterator(): IntIterator = {
    new IntIterator() {

      var gone: Boolean = false

      def hasNext(): Boolean = !gone

      def next(): Int = {
        gone = true
        value
      }
    }
  }

  def union(other: IntSet): IntSet = {
    val n = other.mutableCopy()
    n.add(value)
    n
  }

  def intersect(other: IntSet): IntSet = {
    if (other.contains(value)) {
      this
    } else {
      new IntHashSet()
    }
  }

  def except(other: IntSet): IntSet = {
    if (other.contains(value)) {
      new IntHashSet()
    } else {
      this
    }
  }

  def containsAll(other: IntSet): Boolean = {
    if (other.size > 1) {
      return false
    }
    val ii = other.iterator()
    while (ii.hasNext) {
      if (value != ii.next()) {
        return false
      }
    }
    true
  }
}

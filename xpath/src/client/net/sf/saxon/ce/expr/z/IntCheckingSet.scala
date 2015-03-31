// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

/**
 * An immutable integer set where membership is tested algorithmically
 */
abstract class IntCheckingSet extends IntSet {

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntCheckingSet is immutable")
  }

  def copy(): IntSet = this

  def mutableCopy(): IntSet = {
    throw new UnsupportedOperationException("IntCheckingSet cannot be copied")
  }

  def size(): Int = Integer.MAX_VALUE

  def isEmpty(): Boolean = false

  def contains(value: Int): Boolean

  def remove(value: Int): Boolean = {
    throw new UnsupportedOperationException("IntCheckingSet is immutable")
  }

  def add(value: Int): Boolean = {
    throw new UnsupportedOperationException("IntCheckingSet is immutable")
  }

  def iterator(): IntIterator = {
    throw new UnsupportedOperationException("Cannot iterate over IntCheckingSet")
  }

  def union(other: IntSet): IntSet = {
    val is = this
    new IntCheckingSet() {

      override def contains(value: Int): Boolean = {
        is.contains(value) || other.contains(value)
      }
    }
  }

  def intersect(other: IntSet): IntSet = {
    val is = this
    new IntCheckingSet() {

      override def contains(value: Int): Boolean = {
        is.contains(value) && other.contains(value)
      }
    }
  }

  def except(other: IntSet): IntSet = {
    val is = this
    new IntCheckingSet() {

      override def contains(value: Int): Boolean = {
        is.contains(value) && !other.contains(value)
      }
    }
  }

  def containsAll(other: IntSet): Boolean = {
    val ii = other.iterator()
    while (ii.hasNext) {
      if (!contains(ii.next())) {
        return false
      }
    }
    true
  }
}

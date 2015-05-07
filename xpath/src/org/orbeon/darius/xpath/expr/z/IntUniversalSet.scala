// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.z

object IntUniversalSet {
  val getInstance = new IntUniversalSet()
}

/**
 * An immutable integer set containing every integer
 */
class IntUniversalSet private () extends IntSet {

  def copy(): IntSet = this

  def mutableCopy(): IntSet = new IntComplementSet(new IntHashSet())

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntUniversalSet is immutable")
  }

  def size(): Int = Integer.MAX_VALUE

  def isEmpty: Boolean = false

  def contains(value: Int): Boolean = true

  def remove(value: Int): Boolean = {
    throw new UnsupportedOperationException("IntUniversalSet is immutable")
  }

  def add(value: Int): Boolean = {
    throw new UnsupportedOperationException("IntUniversalSet is immutable")
  }

  def iterator(): IntIterator = {
    throw new UnsupportedOperationException("Cannot enumerate an infinite set")
  }

  def union(other: IntSet): IntSet = this

  def intersect(other: IntSet): IntSet = other.copy()

  def except(other: IntSet): IntSet = {
    if (other.isInstanceOf[IntUniversalSet]) {
      new IntHashSet()
    } else {
      new IntComplementSet(other.copy())
    }
  }

  def containsAll(other: IntSet): Boolean = true
}

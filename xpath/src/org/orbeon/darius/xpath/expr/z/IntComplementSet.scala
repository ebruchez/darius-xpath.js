// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.z

/**
 * An immutable integer set containing all int values except those in an excluded set
 */
class IntComplementSet(_exclusions: IntSet) extends IntSet {

  private val exclusions: IntSet = _exclusions.copy()

  def copy(): IntSet = new IntComplementSet(exclusions)

  def mutableCopy(): IntSet = copy()

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntComplementSet cannot be emptied")
  }

  def size(): Int = Integer.MAX_VALUE - exclusions.size

  def isEmpty: Boolean = size != 0

  def contains(value: Int): Boolean = !exclusions.contains(value)

  def remove(value: Int): Boolean = {
    val b = contains(value)
    if (b) {
      exclusions.add(value)
    }
    b
  }

  def add(value: Int): Boolean = {
    val b = contains(value)
    if (!b) {
      exclusions.remove(value)
    }
    b
  }

  def iterator(): IntIterator = {
    throw new UnsupportedOperationException("Cannot enumerate an infinite set")
  }

  def union(other: IntSet): IntSet = {
    new IntComplementSet(exclusions.except(other))
  }

  def intersect(other: IntSet): IntSet = {
    if (other.isEmpty) {
      new IntHashSet()
    } else if (other == IntUniversalSet.getInstance) {
      copy()
    } else other match {
      case set: IntComplementSet ⇒
        new IntComplementSet(exclusions.union(set.exclusions))
      case _ ⇒
        other.intersect(this)
    }
  }

  def except(other: IntSet): IntSet = {
    new IntComplementSet(exclusions.union(other))
  }

  def containsAll(other: IntSet): Boolean = {
    if (other.size > 1) {
      return false
    }
    val ii = other.iterator()
    while (ii.hasNext) {
      if (exclusions.contains(ii.next())) {
        return false
      }
    }
    true
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

/**
 * Abstract superclass containing helper methods for various implementations of IntSet
 */
abstract class AbstractIntSet extends IntSet {

  /**
   * Test if this set is a superset of another set
   * @param other the other set
   * @return true if every item in the other set is also in this set
   */
  def containsAll(other: IntSet): Boolean = {
    if (other == IntUniversalSet.getInstance || other.isInstanceOf[IntComplementSet]) {
      return false
    }
    val it = other.iterator()
    while (it.hasNext) {
      if (!contains(it.next())) {
        return false
      }
    }
    true
  }

  /**
   * Form a new set that is the union of two IntSets.
   * @param other the second set
   * @return the union of the two sets
   */
  def union(other: IntSet): IntSet = {
    if (other == IntUniversalSet.getInstance) {
      return other
    }
    if (this.isEmpty) {
      return other.copy()
    }
    if (other.isEmpty) {
      return this.copy()
    }
    if (other.isInstanceOf[IntComplementSet]) {
      return other.union(this)
    }
    if (other.isInstanceOf[IntCheckingSet]) {
      return other.union(this)
    }
    val n = new IntHashSet(this.size + other.size)
    var it = iterator()
    while (it.hasNext) {
      n.add(it.next())
    }
    it = other.iterator()
    while (it.hasNext) {
      n.add(it.next())
    }
    n
  }

  /**
   * Form a new set that is the intersection of two IntSets.
   * @param other the second set
   * @return the intersection of the two sets
   */
  def intersect(other: IntSet): IntSet = {
    if (this.isEmpty || other.isEmpty) {
      return new IntHashSet()
    }
    val n = new IntHashSet(size)
    val it = iterator()
    while (it.hasNext) {
      val v = it.next()
      if (other.contains(v)) {
        n.add(v)
      }
    }
    n
  }

  /**
   * Form a new set that is the difference of this set and another set.
   * The result will either be an immutable object, or a newly constructed object.
   * @param other the second set
   * @return the intersection of the two sets
   */
  def except(other: IntSet): IntSet = {
    val n = new IntHashSet(size)
    val it = iterator()
    while (it.hasNext) {
      val v = it.next()
      if (!other.contains(v)) {
        n.add(v)
      }
    }
    n
  }
}

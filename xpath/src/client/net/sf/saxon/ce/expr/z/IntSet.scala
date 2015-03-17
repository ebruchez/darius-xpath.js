// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

/**
 * A set of integers represented as int values
 */
trait IntSet {

  /**
   * Create a copy of this IntSet that leaves the original unchanged.
   * @return an IntSet containing the same integers. The result will not necessarily be the
   * same class as the original. It will either be an immutable object, or a newly constructed
   * object.
   */
  def copy(): IntSet

  /**
   * Create a copy of this IntSet that contains the same set of integers.
   * @return an IntSet containing the same integers. The result will not necessarily be the
   * same class as the original. It will always be a mutable object
   */
  def mutableCopy(): IntSet

  /**
   * Clear the contents of the IntSet (making it an empty set)
   */
  def clear(): Unit

  /**
   * Get the number of integers in the set
   * @return the size of the set
   */
  def size(): Int

  /**
   * Determine if the set is empty
   * @return true if the set is empty, false if not
   */
  def isEmpty(): Boolean

  /**
   * Determine whether a particular integer is present in the set
   * @param value the integer under test
   * @return true if value is present in the set, false if not
   */
  def contains(value: Int): Boolean

  /**
   * Remove an integer from the set
   * @param value the integer to be removed
   * @return true if the integer was present in the set, false if it was not present
   */
  def remove(value: Int): Boolean

  /**
   * Add an integer to the set
   * @param value the integer to be added
   * @return true if the integer was added, false if it was already present
   */
  def add(value: Int): Boolean

  /**
   * Get an iterator over the values
   * @return an iterator over the integers in the set
   */
  def iterator(): IntIterator

  /**
   * Form a new set that is the union of this IntSet and another.
   * The result will either be an immutable object, or a newly constructed object.
   * @param other the second set
   * @return the union of the two sets
   */
  def union(other: IntSet): IntSet

  /**
   * Form a new set that is the intersection of this IntSet and another.
   * The result will either be an immutable object, or a newly constructed object.
   * @param other the second set
   * @return the intersection of the two sets
   */
  def intersect(other: IntSet): IntSet

  /**
   * Form a new set that is the difference of this set and another set.
   * The result will either be an immutable object, or a newly constructed object.
   * @param other the second set
   * @return the intersection of the two sets
   */
  def except(other: IntSet): IntSet

  /**
   * Test if this set is a superset of another set
   * @param other the other set
   * @return true if every integer in the other set is also present in this set
   */
  def containsAll(other: IntSet): Boolean
}

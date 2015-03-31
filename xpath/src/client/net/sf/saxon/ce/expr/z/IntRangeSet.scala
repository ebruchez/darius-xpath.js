// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

import java.io.Serializable

import client.net.sf.saxon.ce.tree.util.FastStringBuffer

import scala.beans.BeanProperty

/**
 * Set of int values. This implementation of IntSet uses a sorted array
 * of integer ranges.
 *
 * @author Michael Kay
 */
class IntRangeSet extends AbstractIntSet with Serializable with IntSet {

  @BeanProperty
  var startPoints: Array[Int] = new Array[Int](4)

  @BeanProperty
  var endPoints: Array[Int] = new Array[Int](4)

  private var used: Int = 0

  private var _hashCode: Int = -1

  var size: Int = 0

  /**
   * Create one IntRangeSet as a copy of another
   * @param input the IntRangeSet to be copied
   */
  def this(input: IntRangeSet) {
    this()
    startPoints = new Array[Int](input.used)
    endPoints = new Array[Int](input.used)
    used = input.used
    System.arraycopy(input.startPoints, 0, startPoints, 0, used)
    System.arraycopy(input.endPoints, 0, endPoints, 0, used)
    _hashCode = input._hashCode
  }

  /**
   * Create an IntRangeSet given the start points and end points of the integer ranges.
   * The two arrays must be the same length; each must be in ascending order; and the n'th end point
   * must be greater than the n'th start point, and less than the n+1'th start point, for all n.
   * @param startPoints the start points of the integer ranges
   * @param endPoints the end points of the integer ranges
   * @throws IllegalArgumentException if the two arrays are different lengths. Other error conditions
   * in the input are not currently detected.
   */
  def this(startPoints: Array[Int], endPoints: Array[Int]) {
    this()
    if (startPoints.length != endPoints.length) {
      throw new IllegalArgumentException("Array lengths differ")
    }
    this.startPoints = startPoints
    this.endPoints = endPoints
    used = startPoints.length
    for (i ← 0 until used) {
      size += (endPoints(i) - startPoints(i) + 1)
    }
  }

  def clear(): Unit = {
    startPoints = new Array[Int](4)
    endPoints = new Array[Int](4)
    used = 0
    _hashCode = -1
  }

  def copy(): IntSet = {
    val s = new IntRangeSet()
    s.startPoints = new Array[Int](startPoints.length)
    System.arraycopy(startPoints, 0, s.startPoints, 0, startPoints.length)
    s.endPoints = new Array[Int](endPoints.length)
    System.arraycopy(endPoints, 0, s.endPoints, 0, endPoints.length)
    s.used = used
    s.size = size
    s
  }

  def mutableCopy(): IntSet = copy()

  def isEmpty: Boolean = size == 0

  def contains(value: Int): Boolean = {
    if (used == 0) {
      return false
    }
    if (value > endPoints(used - 1)) {
      return false
    }
    if (value < startPoints(0)) {
      return false
    }
    var i = 0
    var j = used
    do {
      val mid = i + (j - i) / 2
      if (endPoints(mid) < value) {
        i = Math.max(mid, i + 1)
      } else if (startPoints(mid) > value) {
        j = Math.min(mid, j - 1)
      } else {
        return true
      }
    } while (i != j);
    false
  }

  def remove(value: Int): Boolean = {
    throw new UnsupportedOperationException("remove")
  }

  /**
   * Add an integer to the set
   * @param value the integer to be added
   * @return true if the integer was added, false if it was already present
   */
  def add(value: Int): Boolean = {
    _hashCode = -1
    if (used == 0) {
      ensureCapacity(1)
      startPoints(used - 1) = value
      endPoints(used - 1) = value
      size += 1
      return true
    }
    if (value > endPoints(used - 1)) {
      if (value == endPoints(used - 1) + 1) {
        endPoints(used - 1) += 1
      } else {
        ensureCapacity(used + 1)
        startPoints(used - 1) = value
        endPoints(used - 1) = value
      }
      size += 1
      return true
    }
    if (value < startPoints(0)) {
      if (value == startPoints(0) - 1) {
        startPoints(0) -= 1
      } else {
        ensureCapacity(used + 1)
        System.arraycopy(startPoints, 0, startPoints, 1, used - 1)
        System.arraycopy(endPoints, 0, endPoints, 1, used - 1)
        startPoints(0) = value
        endPoints(0) = value
      }
      size += 1
      return true
    }
    var i = 0
    var j = used
    do {
      val mid = i + (j - i) / 2
      if (endPoints(mid) < value) {
        i = Math.max(mid, i + 1)
      } else if (startPoints(mid) > value) {
        j = Math.min(mid, j - 1)
      } else {
        return false
      }
    } while (i != j);
    if (i > 0 && endPoints(i - 1) + 1 == value) {
      i -= 1
    } else if (i < used - 1 && startPoints(i + 1) - 1 == value) {
      i += 1
    }
    if (endPoints(i) + 1 == value) {
      if (value == startPoints(i + 1) - 1) {
        endPoints(i) = endPoints(i + 1)
        System.arraycopy(startPoints, i + 2, startPoints, i + 1, used - i - 2)
        System.arraycopy(endPoints, i + 2, endPoints, i + 1, used - i - 2)
        used -= 1
      } else {
        endPoints(i) += 1
      }
      size += 1
      true
    } else if (startPoints(i) - 1 == value) {
      if (value == endPoints(i - 1) + 1) {
        endPoints(i - 1) = endPoints(i)
        System.arraycopy(startPoints, i + 1, startPoints, i, used - i - 1)
        System.arraycopy(endPoints, i + 1, endPoints, i, used - i - 1)
        used -= 1
      } else {
        startPoints(i) -= 1
      }
      size += 1
      true
    } else {
      if (value > endPoints(i)) {
        i += 1
      }
      ensureCapacity(used + 1)
      try {
        System.arraycopy(startPoints, i, startPoints, i + 1, used - i - 1)
        System.arraycopy(endPoints, i, endPoints, i + 1, used - i - 1)
      } catch {
        case err: Exception ⇒ err.printStackTrace()
      }
      startPoints(i) = value
      endPoints(i) = value
      size += 1
      true
    }
  }

  private def ensureCapacity(n: Int): Unit = {
    if (startPoints.length < n) {
      val s = new Array[Int](startPoints.length * 2)
      val e = new Array[Int](startPoints.length * 2)
      System.arraycopy(startPoints, 0, s, 0, used)
      System.arraycopy(endPoints, 0, e, 0, used)
      startPoints = s
      endPoints = e
    }
    used = n
  }

  /**
   * Get an iterator over the values
   */
  def iterator(): IntIterator = new IntRangeSetIterator()

  override def toString(): String = {
    val sb = new FastStringBuffer(used * 8)
    for (i ← 0 until used) {
      sb.append(startPoints(i) + "-" + endPoints(i) + ",")
    }
    sb.toString
  }

  /**
   * Test whether this set has exactly the same members as another set. Note that
   * IntRangeSet values are <b>NOT</b> comparable with other implementations of IntSet
   */
  override def equals(other: Any): Boolean = other match {
    case other: IntRangeSet ⇒
      used == other.used && startPoints.sameElements(other.startPoints) && endPoints.sameElements(other.endPoints)
    case other: IntSet ⇒
      containsAll(other)
    case _ ⇒ false
  }

  /**
   * Construct a hash key that supports the equals() test
   */
  override def hashCode(): Int = {
    if (_hashCode == -1) {
      var h = 0x836a89f1
      for (i ← 0 until used) {
        h ^= startPoints(i) + (endPoints(i) << 3)
      }
      _hashCode = h
    }
    _hashCode
  }

  /**
   * Add a range of integers to the set.
   * This is optimized for the case where these are all greater than any existing integer
   * in the set.
   * @param low the low end of the new range
   * @param high the high end of the new range
   */
  def addRange(low: Int, high: Int): Unit = {
    if (low == high) {
      add(low)
      return
    }
    _hashCode = -1
    if (used == 0) {
      ensureCapacity(1)
      startPoints(used - 1) = low
      endPoints(used - 1) = high
      size += (high - low + 1)
    } else if (low > endPoints(used - 1)) {
      if (low == endPoints(used - 1) + 1) {
        endPoints(used - 1) = high
      } else {
        ensureCapacity(used + 1)
        startPoints(used - 1) = low
        endPoints(used - 1) = high
      }
      size += (high - low + 1)
    } else if (high < startPoints(0)) {
      ensureCapacity(used + 1)
      System.arraycopy(startPoints, 0, startPoints, 1, used - 1)
      System.arraycopy(endPoints, 0, endPoints, 1, used - 1)
      startPoints(0) = low
      endPoints(0) = high
    } else {
      for (i ← 1 until used if startPoints(i) > high && endPoints(i - 1) < low) {
        ensureCapacity(used + 1)
        System.arraycopy(startPoints, i, startPoints, i + 1, used - i - 1)
        System.arraycopy(endPoints, i, endPoints, i + 1, used - i - 1)
        startPoints(i) = low
        endPoints(i) = high
        return
      }
      var i = low
      while (i <= high) {
        add(i)
        i += 1
      }
    }
  }

  /**
   * Get the number of ranges actually in use
   */
  def getNumberOfRanges: Int = used

  /**
   * Iterator class
   */
  private class IntRangeSetIterator extends IntIterator with Serializable {

    private var i: Int = -1

    private var current: Int = Integer.MIN_VALUE

    def hasNext: Boolean = {
      if (i < 0) {
        size > 0
      } else {
        current < endPoints(used - 1)
      }
    }

    def next(): Int = {
      if (i < 0) {
        i = 0
        current = startPoints(0)
        return current
      }
      if (current == endPoints(i)) {
        current = startPoints(i)
        current
      } else {
        current
      }
    }
  }
}

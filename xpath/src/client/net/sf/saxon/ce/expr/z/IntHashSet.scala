// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.z

import java.io.Serializable

import client.net.sf.saxon.ce.expr.z.IntHashSet._
import client.net.sf.saxon.ce.tree.util.FastStringBuffer

object IntHashSet {

  private val NBIT = 30

  /**
   * The maximum number of elements this container can contain.
   */
  val MAX_SIZE = 1 << NBIT

  def fromArray(members: Array[Int]): IntHashSet = {
    val ihs = new IntHashSet(members.length)
    for (m ← members) {
      ihs.add(m)
    }
    ihs
  }

  /**
   * Test if one set has overlapping membership with another set
   */
  def containsSome(one: IntSet, two: IntSet): Boolean = {
    val it = two.iterator()
    while (it.hasNext) {
      if (one.contains(it.next())) {
        return true
      }
    }
    false
  }
}

/**
 * Set of int values. This class is modelled on the java.net.Set interface, but it does
 * not implement this interface, because the set members are nint's rather than Objects.
 * <p/>
 * Not thread safe.
 * @author Dominique Devienne
 * @author Michael Kay: retrofitted to JDK 1.4, added iterator()
 */
class IntHashSet(capacity: Int, val ndv: Int) extends AbstractIntSet with IntSet with Serializable {

  private var _nmax: Int = _

  private var _size: Int = _

  private var _nlo: Int = _

  private var _nhi: Int = _

  private var _shift: Int = _

  private var _mask: Int = _

  private var _values: Array[Int] = _

  setCapacity(capacity)

  /**
   * Initializes a set with a capacity of 8 and a load factor of 0,25.
   */
  def this() {
    this(8, Integer.MIN_VALUE)
  }

  /**
   * Initializes a set with the given capacity and a load factor of 0,25.
   * @param capacity the initial capacity.
   */
  def this(capacity: Int) {
    this(capacity, Integer.MIN_VALUE)
  }

  def copy(): IntSet = {
    if (_size == 0) {
      new IntHashSet()
    } else {
      val s = new IntHashSet(_size, ndv)
      s._nmax = _nmax
      s._size = _size
      s._nlo = _nlo
      s._nhi = _nhi
      s._shift = _shift
      s._size = _size
      s._values = new Array[Int](_values.length)
      System.arraycopy(_values, 0, s._values, 0, _values.length)
      s
    }
  }

  def mutableCopy(): IntSet = copy()

  def clear(): Unit = {
    _size = 0
    for (i ← 0 until _nmax) {
      _values(i) = ndv
    }
  }

  def size(): Int = _size

  def isEmpty(): Boolean = _size == 0

  def getValues: Array[Int] = {
    var index = 0
    val values = new Array[Int](_size)
    for (_value ← _values if _value != ndv) {
      values(index) = _value
      index += 1
    }
    values
  }

  def contains(value: Int): Boolean = _values(indexOf(value)) != ndv

  def remove(value: Int): Boolean = {
    var i = indexOf(value)
    if (_values(i) == ndv) {
      return false
    }
    _size -= 1
    while (true) {
      _values(i) = ndv
      val j = i
      var r: Int = 0
      do {
        i = (i - 1) & _mask
        if (_values(i) == ndv) {
          return true
        }
        r = hash(_values(i))
      } while ((i <= r && r < j) || (r < j && j < i) || (j < i && i <= r));
      _values(j) = _values(i)
    }
    throw new IllegalStateException()
  }

  def add(value: Int): Boolean = {
    if (value == ndv) {
      throw new IllegalArgumentException("Can't add the 'no data' value")
    }
    val i = indexOf(value)
    if (_values(i) == ndv) {
      _size += 1
      _values(i) = value
      if (_size > MAX_SIZE) {
        throw new RuntimeException("Too many elements (> " + MAX_SIZE + ')')
      }
      if (_nlo < _size && _size <= _nhi) {
        setCapacity(_size)
      }
      true
    } else {
      false
    }
  }

  private def hash(key: Int): Int = ((1327217885 * key) >> _shift) & _mask

  /**
   * Gets the index of the value, if it exists, or the index at which
   * this value would be added if it does not exist yet.
   */
  private def indexOf(value: Int): Int = {
    var i = hash(value)
    while (_values(i) != ndv) {
      if (_values(i) == value) {
        return i
      }
      i = (i - 1) & _mask
    }
    i
  }

  private def setCapacity(_capacity: Int): Unit = {
    var capacity = _capacity
    if (capacity < _size) {
      capacity = _size
    }
    var nbit: Int = 0
    var nmax: Int = 0
    nbit = 1
    nmax = 2
    while (nmax < capacity * 4 && nmax < MAX_SIZE) {

      nbit
      nmax *= 2
    }
    val nold = _nmax
    if (nmax == nold) {
      return
    }
    _nmax = nmax
    _nlo = nmax / 4
    _nhi = MAX_SIZE / 4
    _shift = 1 + NBIT - nbit
    _mask = nmax - 1
    _size = 0
    val values = _values
    _values = new Array[Int](nmax)
    java.util.Arrays.fill(_values, ndv)
    if (values != null) {
      for (i ← 0 until nold) {
        val value = values(i)
        if (value != ndv) {
          _size += 1
          _values(indexOf(value)) = value
        }
      }
    }
  }

  /**
   * Get an iterator over the values
   */
  def iterator(): IntIterator = new IntHashSetIterator()

  /**
   * Test whether this set has exactly the same members as another set
   */
  override def equals(other: Any): Boolean = other match {
    case other: IntSet ⇒
      val s = other
      size == s.size && containsAll(s)
    case _ ⇒ false
  }

  /**
   * Construct a hash key that supports the equals() test
   */
  override def hashCode(): Int = {
    var h = 936247625
    val it = iterator()
    while (it.hasNext) {
      h += it.next()
    }
    h
  }

  /**
   * Diagnostic output
   */
  def diagnosticDump(): Unit = {
    System.err.println("Contents of IntHashSet")
    val sb = new FastStringBuffer(100)
    for (i ← 0 until _values.length) {
      if (i % 10 == 0) {
        System.err.println(sb.toString)
        sb.setLength(0)
      }
      if (_values(i) == ndv) {
        sb.append("*, ")
      } else {
        sb.append(_values(i) + ", ")
      }
    }
    System.err.println(sb.toString)
    sb.setLength(0)
    System.err.println("size: " + _size)
    System.err.println("ndv: " + ndv)
    System.err.println("nlo: " + _nlo)
    System.err.println("nhi: " + _nhi)
    System.err.println("nmax: " + _nmax)
    System.err.println("shift: " + _shift)
    System.err.println("mask: " + _mask)
    System.err.println("Result of iterator:")
    val iter = iterator()
    var i = 0
    while (iter.hasNext) {
      if (i % 10 == 0) {
        System.err.println(sb.toString)
        sb.setLength(0)
      }
      i += 1
      sb.append(iter.next() + ", ")
    }
    System.err.println(sb.toString)
    System.err.println("=====================")
  }

  /**
   * Iterator class
   * @author Saxonica Limited
   */
  private class IntHashSetIterator extends IntIterator with Serializable {

    private var i: Int = 0

    def hasNext(): Boolean = {
      while (i < _values.length) {
        if (_values(i) != ndv) {
          return true
        } else {
          i += 1
        }
      }
      false
    }

    def next(): Int = {
      val result = _values(i)
      i += 1
      result
    }
  }
}

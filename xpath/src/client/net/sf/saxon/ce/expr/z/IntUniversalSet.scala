package client.net.sf.saxon.ce.expr.z

import IntUniversalSet._
//remove if not needed
import scala.collection.JavaConversions._

object IntUniversalSet {

  private var THE_INSTANCE: IntUniversalSet = new IntUniversalSet()

  def getInstance(): IntUniversalSet = THE_INSTANCE
}

/**
 * An immutable integer set containing every integer
 */
class IntUniversalSet private () extends IntSet {

  def copy(): IntSet = this

  def mutableCopy(): IntSet = new IntComplementSet(new IntHashSet())

  def clear() {
    throw new UnsupportedOperationException("IntUniversalSet is immutable")
  }

  def size(): Int = Integer.MAX_VALUE

  def isEmpty(): Boolean = false

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

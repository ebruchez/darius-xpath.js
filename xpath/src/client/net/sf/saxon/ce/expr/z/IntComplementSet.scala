package client.net.sf.saxon.ce.expr.z

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An immutable integer set containing all int values except those in an excluded set
 */
class IntComplementSet(exclusions: IntSet) extends IntSet {

  private var exclusions: IntSet = exclusions.copy()

  def copy(): IntSet = new IntComplementSet(exclusions)

  def mutableCopy(): IntSet = copy()

  def clear() {
    throw new UnsupportedOperationException("IntComplementSet cannot be emptied")
  }

  def size(): Int = Integer.MAX_VALUE - exclusions.size

  def isEmpty(): Boolean = size != 0

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
    } else if (other.isInstanceOf[IntComplementSet]) {
      new IntComplementSet(exclusions.union(other.asInstanceOf[IntComplementSet].exclusions))
    } else {
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

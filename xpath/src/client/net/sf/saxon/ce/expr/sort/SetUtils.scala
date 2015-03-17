package client.net.sf.saxon.ce.expr.sort

import java.util.HashSet
import java.util.Iterator
import java.util.Set
//remove if not needed
import scala.collection.JavaConversions._

object SetUtils {

  /**
   * Form a new set that is the union of two IntSets.
   */
  def union(one: Set[_], two: Set[_]): Set[_] = {
    val n = new HashSet(one.size + two.size)
    var it = one.iterator()
    while (it.hasNext) {
      n.add(it.next())
    }
    it = two.iterator()
    while (it.hasNext) {
      n.add(it.next())
    }
    n
  }

  /**
   * Form a new set that is the intersection of one set with another set.
   */
  def intersect(one: Set[_], two: Set[_]): Set[_] = {
    val n = new HashSet(one.size)
    val it = one.iterator()
    while (it.hasNext) {
      val v = it.next()
      if (two.contains(v)) {
        n.add(v)
      }
    }
    n
  }

  /**
   * Form a new set that is the difference of one set with another set.
   */
  def except(one: Set[_], two: Set[_]): Set[_] = {
    val n = new HashSet(one.size)
    val it = one.iterator()
    while (it.hasNext) {
      val v = it.next()
      if (!two.contains(v)) {
        n.add(v)
      }
    }
    n
  }

  /**
   * Test if one set has overlapping membership with another set
   */
  def containsSome(one: Set[_], two: Set[_]): Boolean = {
    val it = two.iterator()
    while (it.hasNext) {
      if (one.contains(it.next())) {
        return true
      }
    }
    false
  }
}

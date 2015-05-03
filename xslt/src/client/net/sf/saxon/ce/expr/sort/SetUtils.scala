// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import java.util.{HashSet, Set}

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

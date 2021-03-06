// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.LastPositionFinder
import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.functions.DistinctValues
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.FocusIterator
import org.orbeon.darius.xpath.tree.iter.ListIterator
import org.orbeon.darius.xpath.`type`.AtomicType
import org.orbeon.darius.xpath.value.AtomicValue
import java.util.ArrayList
import java.util.HashMap
import java.util.List

import scala.collection.JavaConversions._

/**
 * A GroupByIterator iterates over a sequence of groups defined by
 * xsl:for-each-group group-by="x". The groups are returned in
 * order of first appearance. Note that an item can appear in several groups;
 * indeed, an item may be the leading item of more than one group, which means
 * that knowing the leading item is not enough to know the current group.
 *
 * <p>The GroupByIterator acts as a SequenceIterator, where successive calls of
 * next() return the leading item of each group in turn. The current item of
 * the iterator is therefore the leading item of the current group. To get access
 * to all the members of the current group, the method iterateCurrentGroup() is used;
 * this underpins the current-group() function in XSLT. The grouping key for the
 * current group is available via the getCurrentGroupingKey() method.</p>
 */
class GroupByIterator(population: SequenceIterator, 
    protected var keyExpression: Expression, 
    var keyContext: XPathContext, 
    var collator: StringCollator) extends GroupIterator with LastPositionFinder {

  private var population: FocusIterator = c2.setCurrentIterator(population)

  private var position: Int = 0

  protected var groups: List[List[Item]] = new ArrayList[List[Item]](40)

  protected var groupKeys: List[AtomicValue] = new ArrayList[AtomicValue](40)

  protected var comparer: AtomicComparer = AtomicSortComparer.makeSortComparer(collator, `type`, keyContext.getImplicitTimezone)

  val `type` = keyExpression.getItemType.asInstanceOf[AtomicType]

  val index = new HashMap[Any, List[Item]](40)

  val c2 = this.keyContext.newMinorContext()

  while (true) {
    val item = this.population.next()
    if (item == null) {
      //break
    }
    processItem(index, item, c2)
  }

  /**
   * Process one item in the population
   * @param index the index of items
   * @param item the item from the population to be processed
   * @param c2 the XPath evaluation context
   * @throws XPathException on dynamic error
   */
  protected def processItem(index: HashMap[Any, List[Item]], item: Item, c2: XPathContext): Unit = {
    val keys = keyExpression.iterate(c2)
    var firstKey = true
    while (true) {
      val key = keys.next().asInstanceOf[AtomicValue]
      if (key == null) {
        //break
      }
      var comparisonKey: AnyRef = null
      comparisonKey = if (key.isNaN) classOf[DistinctValues] else key.getXPathComparable(ordered = false, collator,
        c2.getImplicitTimezone)
      val g = index.get(comparisonKey)
      if (g == null) {
        val newGroup = new ArrayList[Item](20)
        newGroup.add(item)
        groups.add(newGroup)
        groupKeys.add(key)
        index.put(comparisonKey, newGroup)
      } else {
        if (firstKey) {
          g.add(item)
        } else {
          if (g.get(g.size - 1) != item) {
            g.add(item)
          }
        }
      }
      firstKey = false
    }
  }

  /**
   * Get the value of the grouping key for the current group
   * @return the grouping key, or null if the grouping key is an empty sequence
   */
  def getCurrentGroupingKey: AtomicValue = groupKeys.get(position - 1)

  /**
   * Get an iterator over the items in the current group
   * @return the iterator
   */
  def iterateCurrentGroup(): SequenceIterator = {
    new ListIterator(groups.get(position - 1))
  }

  /**
   * Get the contents of the current group as a java List
   * @return the contents of the current group
   */
  def getCurrentGroup(): List[_] = groups.get(position - 1)

  def next(): Item = {
    if (position >= 0 && position < groups.size) {
      position += 1
      current0()
    } else {
      position = -1
      null
    }
  }

  def current(): Item = current0()

  private def current0(): Item = {
    if (position < 1) {
      return null
    }
    groups.get(position - 1).asInstanceOf[ArrayList].get(0).asInstanceOf[Item]
  }

  def getAnother: SequenceIterator = {
    val c2 = keyContext.newMinorContext()
    new GroupByIterator(population.getAnother, keyExpression, c2, collator)
  }

  /**
   * Get the last position (that is, the number of groups)
   */
  def getLastPosition: Int = groups.size
}

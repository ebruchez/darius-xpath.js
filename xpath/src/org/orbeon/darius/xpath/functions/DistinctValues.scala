// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import java.{util ⇒ ju}

import org.orbeon.darius.xpath.expr.{ItemMappingFunction, ItemMappingIterator, StatefulMappingFunction, XPathContext}
import org.orbeon.darius.xpath.functions.DistinctValues._
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.value.AtomicValue

object DistinctValues {

  class DistinctItemsMappingFunction(var collator: StringCollator, var implicitTimezone: Int)
      extends ItemMappingFunction with StatefulMappingFunction {

    private val lookup: ju.HashSet[Any] = new ju.HashSet[Any](40)

    def mapItem(item: Item): Item = {
      val value = item.asInstanceOf[AtomicValue]
      var key: AnyRef = null
      key = if (value.isNaN) classOf[DistinctValues] else value.getXPathComparable(ordered = false, collator, implicitTimezone)
      if (lookup.add(key)) {
        item
      } else {
        null
      }
    }

    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = {
      new DistinctItemsMappingFunction(collator, implicitTimezone)
    }
  }
}

/**
 * The XPath 2.0 distinct-values() function
 */
class DistinctValues extends CollatingFunction {

  def newInstance(): DistinctValues = new DistinctValues()

  /**
   * Evaluate the function to return an iteration of selected values or nodes.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val collator = getCollator(1, context)
    val iter = argument(0).iterate(context)
    val function = new DistinctItemsMappingFunction(collator, context.getImplicitTimezone)
    new ItemMappingIterator(iter, function)
  }
}

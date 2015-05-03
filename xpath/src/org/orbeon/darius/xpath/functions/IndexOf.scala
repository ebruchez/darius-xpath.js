// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.{AtomicType, Type}
import org.orbeon.darius.xpath.expr.sort.AtomicComparer
import org.orbeon.darius.xpath.expr.{ItemMappingFunction, ItemMappingIterator, StatefulMappingFunction, XPathContext}
import org.orbeon.darius.xpath.functions.IndexOf._
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.value.{AtomicValue, IntegerValue}

object IndexOf {

  class IndexOfMappingFunction(var searchType: AtomicType, var comparer: AtomicComparer, var `val`: AtomicValue)
      extends ItemMappingFunction with StatefulMappingFunction {

    var index: Int = 0

    def mapItem(item: Item): IntegerValue = {
      index += 1
      if (Type.isComparable(searchType, item.asInstanceOf[AtomicValue].getItemType, ordered = false) &&
        comparer.comparesEqual(item.asInstanceOf[AtomicValue], `val`)) {
        new IntegerValue(index)
      } else {
        null
      }
    }

    /**
     * Return a clone of this MappingFunction, with the state reset to its state at the beginning
     * of the underlying iteration
     *
     * @return a clone of this MappingFunction
     * @param newBaseIterator
     */
    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = {
      new IndexOfMappingFunction(searchType, comparer, `val`)
    }
  }
}

/**
 * The XPath 2.0 index-of() function
 */
class IndexOf extends CollatingFunction {

  def newInstance(): IndexOf = new IndexOf()

  /**
   * Evaluate the function to return an iteration of selected items.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val comparer = getAtomicComparer(2, context)
    val seq = argument(0).iterate(context)
    val `val` = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    val searchType = `val`.getItemType
    new ItemMappingIterator(seq, new IndexOfMappingFunction(searchType, comparer, `val`))
  }
}

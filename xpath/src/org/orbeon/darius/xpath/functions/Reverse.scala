// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr.{StaticProperty, XPathContext}
import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.value.SequenceExtent

/**
 * Implement XPath function fn:reverse()
 */
class Reverse extends SystemFunction {

  def newInstance(): Reverse = new Reverse()

  /**
   * Determine the item type of the value returned by the function
   *
   */
  override def getItemType: ItemType = argument(0).getItemType

  override def computeSpecialProperties(): Int = {
    val baseProps = argument(0).getSpecialProperties
    if ((baseProps & StaticProperty.REVERSE_DOCUMENT_ORDER) != 
      0) {
      (baseProps & (~StaticProperty.REVERSE_DOCUMENT_ORDER)) | 
        StaticProperty.ORDERED_NODESET
    } else if ((baseProps & StaticProperty.ORDERED_NODESET) != 0) {
      (baseProps & (~StaticProperty.ORDERED_NODESET)) | StaticProperty.REVERSE_DOCUMENT_ORDER
    } else {
      baseProps
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val forwards = argument(0).iterate(context)
    val extent = SequenceExtent.makeReversed(forwards)
    extent.iterate()
  }
}

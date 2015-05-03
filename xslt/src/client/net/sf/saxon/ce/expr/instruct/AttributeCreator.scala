// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.event.SequenceReceiver
import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.Whitespace
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract class for fixed and computed attribute constructor expressions
 */
abstract class AttributeCreator extends SimpleNodeConstructor {

  /**
   * Process the value of the node, to create the new node.
   * @param value the string value of the new node
   * @param context the dynamic evaluation context
   * @throws XPathException
   */
  def processValue(value: CharSequence, context: XPathContext): Unit = {
    val nameCode = evaluateNameCode(context)
    val out = context.getReceiver
    if (nameCode == StructuredQName.XML_ID) {
      value = Whitespace.collapseWhitespace(value)
    }
    out.attribute(nameCode, value)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.value.{AtomicValue, SequenceType, StringValue}

class Concat extends SystemFunction {

  def newInstance(): Concat = new Concat()

  /**
   * Get the required type of the nth argument
   */
  override protected def getRequiredType(arg: Int): SequenceType = getDetails.argumentTypes(0)

  /**
   * Evaluate the function in a string context
   */
  override def evaluateAsString(c: XPathContext): CharSequence = evaluateItem(c).getStringValue

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = {
    val numArgs = argument.length
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    for (i ← 0 until numArgs) {
      val `val` = argument(i).evaluateItem(c).asInstanceOf[AtomicValue]
      if (`val` != null) {
        sb.append(`val`.getStringValue)
      }
    }
    StringValue.makeStringValue(sb.condense())
  }
}

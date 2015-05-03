// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.expr.sort.CodepointCollator
import org.orbeon.darius.xpath.functions.Contains._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value._

object Contains {
  val CONTAINS = 0
  val STARTS_WITH = 1
  val ENDS_WITH = 2
}

/**
 * Implements the fn:contains() function, also starts-with() and ends-with()
 */
class Contains(op: Int) extends CollatingFunction {

  operation = op

  def newInstance(): Contains = new Contains(operation)

  override def evaluateItem(context: XPathContext): Item = {
    var result: Boolean = false
    val arg1 = argument(1).evaluateItem(context).asInstanceOf[StringValue]
    if (arg1 == null || arg1.isZeroLength) {
      result = true
    } else {
      val arg0 = argument(0).evaluateItem(context).asInstanceOf[StringValue]
      if (arg0 == null || arg0.isZeroLength) {
        result = false
      } else {
        val s0 = arg0.getStringValue
        val s1 = arg1.getStringValue
        val collator = getCollator(2, context)
        if (collator.isInstanceOf[CodepointCollator]) operation match {
          case CONTAINS ⇒ result = s0.indexOf(s1, 0) >= 0
          case STARTS_WITH ⇒ result = s0.startsWith(s1, 0)
          case ENDS_WITH ⇒ result = s0.endsWith(s1)
        } else {
          doesNotSupportSubstringMatching(context)
          result = false
        }
      }
    }
    BooleanValue.get(result)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.StaticProperty
import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.regex.ARegexIterator
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.AtomicValue
import org.orbeon.darius.xpath.value.NumericValue
import org.orbeon.darius.xpath.value.StringValue

import scala.collection.JavaConversions._

class RegexGroup extends SystemFunction {

  def newInstance(): RegexGroup = new RegexGroup()

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    val gp0 = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    val gp = gp0.asInstanceOf[NumericValue]
    val iter = c.getCurrentRegexIterator
    if (iter == null) {
      return StringValue.EMPTY_STRING
    }
    val s = iter.getRegexGroup(gp.intValue())
    if (s == null) {
      return StringValue.EMPTY_STRING
    }
    StringValue.makeStringValue(s)
  }

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_REGEX_GROUP
}

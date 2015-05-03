// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.regex.ARegularExpression
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.{AtomicValue, BooleanValue, StringValue}

/**
 * This class implements the matches() function for regular expression matching
 */
class Matches extends SystemFunction {

  def newInstance(): Matches = new Matches()

  /**
   * Evaluate the matches() function to give a Boolean value.
   * @param c  The dynamic evaluation context
   * @return the result as a BooleanValue, or null to indicate the empty sequence
   * @throws XPathException on an error
   */
  override def evaluateItem(c: XPathContext): Item = {
    var sv0 = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    if (sv0 == null) {
      sv0 = StringValue.EMPTY_STRING
    }
    val pat = argument(1).evaluateItem(c).asInstanceOf[AtomicValue]
    if (pat == null) return null
    var flags: CharSequence = null
    if (argument.length == 2) {
      flags = ""
    } else {
      val sv2 = argument(2).evaluateItem(c).asInstanceOf[AtomicValue]
      if (sv2 == null) return null
      flags = sv2.getStringValue
    }
    try {
      val re = new ARegularExpression(pat.getStringValue, flags.toString, "XP20", null)
      BooleanValue.get(re.containsMatch(sv0.getStringValue))
    } catch {
      case err: XPathException ⇒
        val de = new XPathException(err)
        de.maybeSetErrorCode("FORX0002")
        throw de
    }
  }
}

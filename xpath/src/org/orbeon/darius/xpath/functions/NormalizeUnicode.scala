// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.functions.codenorm.Normalizer
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value.{StringValue, Whitespace}

import scala.util.control.Breaks

/**
 * Implement the XPath normalize-unicode() function
 */
class NormalizeUnicode extends SystemFunction {

  def newInstance(): NormalizeUnicode = new NormalizeUnicode()

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = {
    val sv = argument(0).evaluateItem(c).asInstanceOf[StringValue]
    if (sv == null) {
      return StringValue.EMPTY_STRING
    }
    var fb = Normalizer.C
    if (argument.length == 2) {
      val form = Whitespace.trim(argument(1).evaluateAsString(c))
      if (form.equalsIgnoreCase("NFC")) {
        fb = Normalizer.C
      } else if (form.equalsIgnoreCase("NFD")) {
        fb = Normalizer.D
      } else if (form.equalsIgnoreCase("NFKC")) {
        fb = Normalizer.KC
      } else if (form.equalsIgnoreCase("NFKD")) {
        fb = Normalizer.KD
      } else if (form.length == 0) {
        return sv
      } else {
        dynamicError("Normalization form " + form + " is not supported", "FOCH0003")
      }
    }
    var allASCII = true
    val chars = sv.getStringValue
    var i = chars.length - 1
    import Breaks._
    breakable {
      while (i >= 0) {
        if (chars.charAt(i) > 127) {
          allASCII = false
          break()
        }
        i -= 1
      }
    }
    if (allASCII) {
      return sv
    }
    val norm = new Normalizer(fb, c.getConfiguration)
    val result = norm.normalize(sv.getStringValue)
    StringValue.makeStringValue(result)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.{DoubleValue, StringValue}

/**
 * This class implements the XPath substring() function
 */
class Substring extends SystemFunction {

  def newInstance(): Substring = new Substring()

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val sv = argument(0).evaluateItem(context).asInstanceOf[StringValue]
    if (sv == null) {
      return StringValue.EMPTY_STRING
    }
    val str = sv.getStringValue
    val start = argument(1).evaluateItem(context).asInstanceOf[DoubleValue]
      .round()
      .getDoubleValue
    var length: Double = 0.0
    if (argument.length == 2) {
      length = str.length.toDouble
    } else {
      length = argument(2).evaluateItem(context).asInstanceOf[DoubleValue]
        .round()
        .getDoubleValue
      if (length < 0) {
        length = 0
      }
    }
    val sb = new FastStringBuffer(length.toInt)
    var i = 0
    var pos = 0
    while (i < start - 1 && pos < str.length) {
      val c = str.charAt(pos).toInt
      pos += 1
      if (c < 55296 || c > 56319) i += 1
    }
    var j = 0
    while (j < length && pos < str.length) {
      val c = str.charAt(pos)
      pos += 1
      sb.append(c)
      if (c.toInt < 55296 || c.toInt > 56319) j += 1
    }
    val result = new StringValue(sb)
    if (sv.isKnownToContainNoSurrogates) {
      result.setContainsNoSurrogates()
    }
    result
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.{Item, NameChecker}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.{FastStringBuffer, UTF16CharacterSet}
import org.orbeon.darius.xpath.value.{NumericValue, StringValue}

/**
 * This class supports the function codepoints-to-string
 */
class CodepointsToString extends SystemFunction {

  def newInstance(): CodepointsToString = new CodepointsToString()

  /**
   * Evaluate
   */
  override def evaluateItem(c: XPathContext): Item = {
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    val si = argument(0).iterate(c)
    while (true) {
      val nextInt = si.next().asInstanceOf[NumericValue]
      if (nextInt == null) {
        return StringValue.makeStringValue(sb.condense())
      }
      val next = nextInt.intValue()
      if (next < 0 || next > Integer.MAX_VALUE || ! NameChecker.isValidChar(next.toInt)) {
        throw new XPathException(s"Invalid XML character [x ${Integer.toHexString(next.toInt)}]", "FOCH0001")
      }
      if (next < 65536) {
        sb.append(next.toChar)
      } else {
        sb.append(UTF16CharacterSet.highSurrogate(next.toInt))
        sb.append(UTF16CharacterSet.lowSurrogate(next.toInt))
      }
    }
    throw new IllegalStateException
  }
}

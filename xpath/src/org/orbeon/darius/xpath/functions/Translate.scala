// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.value.StringValue

import scala.util.control.Breaks

/**
 * Implement the XPath translate() function
 */
class Translate extends SystemFunction {

  def newInstance(): Translate = new Translate()

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val sv1 = argument(0).evaluateItem(context).asInstanceOf[StringValue]
    if (sv1 == null) {
      return StringValue.EMPTY_STRING
    }
    val sv2 = argument(1).evaluateItem(context).asInstanceOf[StringValue]
    val sv3 = argument(2).evaluateItem(context).asInstanceOf[StringValue]
    val a1 = sv1.expand()
    val a2 = sv2.expand()
    val a3 = sv3.expand()
    val length1 = a1.length
    val length2 = a2.length
    val sb = new FastStringBuffer(length1)

    for (i ← 0 until length1) {
      val ch = a1(i)
      import Breaks._
      breakable {
        for (j ← 0 until length2) {
          if (a2(j) == ch) {
            if (j < a3.length) {
              sb.appendWideChar(a3(j))
            } else {
              // do nothing, delete the character
            }
            break()// after sb.appendWideChar(ch) below
          }
        }
        sb.appendWideChar(ch)
      }
    }
    StringValue.makeStringValue(sb)
  }
}

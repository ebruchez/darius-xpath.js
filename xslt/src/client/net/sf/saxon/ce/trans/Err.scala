// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.trans

import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
//remove if not needed
import scala.collection.JavaConversions._

object Err {

  val ELEMENT = 1

  val ATTRIBUTE = 2

  val FUNCTION = 3

  val VALUE = 4

  val VARIABLE = 5

  val GENERAL = 6

  val URI = 7

  /**
   * Add delimiters to represent variable information within an error message
   * @param cs the variable information to be delimited
   * @return the delimited variable information
   */
  def wrap(cs: CharSequence): String = wrap(cs, GENERAL)

  /**
   * Add delimiters to represent variable information within an error message
   * @param cs the variable information to be delimited
   * @param valueType the type of value, e.g. element name or attribute name
   * @return the delimited variable information
   */
  def wrap(cs: CharSequence, valueType: Int): String = {
    if (cs == null) {
      return "(NULL)"
    }
    if (cs.length == 0) {
      return "(zero-length-string)"
    }
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    val len = cs.length
    for (i ← 0 until len) {
      val c = cs.charAt(i)
      if (c < 32 || c > 255) {
        sb.append("\\u")
        sb.append(Integer.toHexString(c))
      } else {
        sb.append(c)
      }
    }
    var s: String = null
    if (len > 30) {
      if (valueType == ELEMENT && sb.charAt(0) == '{') {
        val qn = StructuredQName.fromClarkName(sb.toString)
        var uri = qn.getNamespaceURI
        if (uri.length > 15) {
          uri = "..." + uri.substring(uri.length - 15)
        }
        s = "{" + uri + "}" + qn.getLocalName
      } else s = if (valueType == URI) "..." + sb.toString.substring(len - 30) else sb.toString.substring(0, 
        30) + "..."
    } else {
      s = sb.toString
    }
    valueType match {
      case ELEMENT ⇒ "<" + s + ">"
      case ATTRIBUTE ⇒ "@" + s
      case FUNCTION ⇒ s + "()"
      case VARIABLE ⇒ "$" + s
      case VALUE ⇒ "\"" + s + "\""
      case _ ⇒ "{" + s + "}"
    }
  }
}

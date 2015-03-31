// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.Replace._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.regex.ARegularExpression
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{AtomicValue, StringValue}

object Replace {

  /**
   * Check the contents of the replacement string
   *
   * @param rep the replacement string
   * @return null if the string is OK, or an error message if not
   */
  def checkReplacement(rep: CharSequence): String = {
    for (i ← 0 until rep.length) {
      val c = rep.charAt(i)
      if (c == '$') {
        if (i + 1 < rep.length) {
          val next = rep.charAt(i)
          if (next < '0' || next > '9') {
            return "Invalid replacement string in replace(): $ sign must be followed by digit 0-9"
          }
        } else {
          return "Invalid replacement string in replace(): $ sign at end of string"
        }
      } else if (c == '\\') {
        if (i + 1 < rep.length) {
          val next = rep.charAt(i)
          if (next != '\\' && next != '$') {
            return "Invalid replacement string in replace(): \\ character must be followed by \\ or $"
          }
        } else {
          return "Invalid replacement string in replace(): \\ character at end of string"
        }
      }
    }
    null
  }
}

/**
 * This class implements the replace() function for replacing
 * substrings that match a regular expression
 */
class Replace extends SystemFunction {

  def newInstance(): Replace = new Replace()

  /**
   * Evaluate the function in a string context
   */
  override def evaluateItem(c: XPathContext): Item = {
    var arg0 = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    if (arg0 == null) {
      arg0 = StringValue.EMPTY_STRING
    }
    val arg2 = argument(2).evaluateItem(c).asInstanceOf[AtomicValue]
    val replacement = arg2.getStringValue
    val msg = checkReplacement(replacement)
    if (msg != null) {
      dynamicError(msg, "FORX0004")
    }
    val arg1 = argument(1).evaluateItem(c).asInstanceOf[AtomicValue]
    var flags: CharSequence = null
    if (argument.length == 3) {
      flags = ""
    } else {
      val arg3 = argument(3).evaluateItem(c).asInstanceOf[AtomicValue]
      flags = arg3.getStringValue
    }
    try {
      val re = new ARegularExpression(arg1.getStringValue, flags.toString, "XP20", null)
      if (re.matches("")) {
        dynamicError("The regular expression in replace() must not be one that matches a zero-length string", 
          "FORX0003")
      }
      val input = arg0.getStringValue
      val res = re.replace(input, replacement)
      StringValue.makeStringValue(res)
    } catch {
      case err: XPathException ⇒
        val de = new XPathException(err)
        de.setErrorCode("FORX0002")
        de.maybeSetLocation(getSourceLocator)
        throw de
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import scala.beans.BooleanBeanProperty

/**
 * Class representing a set of regular expression flags (some combination of i, m, s, x, q).
 * Also contains options affecting the regular expression dialect: whether or not XPath 2.0
 * and XPath 3.0 extensions to XSD regex syntax are accepted.
 */
class REFlags(flags: String, language: String) {

  @BooleanBeanProperty
  var caseIndependent: Boolean = _

  @BooleanBeanProperty
  var multiLine: Boolean = _

  @BooleanBeanProperty
  var singleLine: Boolean = _

  @BooleanBeanProperty
  var allowWhitespace: Boolean = _

  @BooleanBeanProperty
  var literal: Boolean = _

  private var xpath20: Boolean = _

  private var xpath30: Boolean = _

  @BooleanBeanProperty
  var debug: Boolean = _

  val semi = flags.indexOf(';')

  val endStd = if (semi >= 0) semi else flags.length

  for (i ← 0 until endStd) {
    val c = flags.charAt(i)
    c match {
      case 'i' ⇒ caseIndependent = true
      case 'm' ⇒ multiLine = true
      case 's' ⇒ singleLine = true
      case 'q' ⇒ literal = true
      case 'x' ⇒ allowWhitespace = true
      case _ ⇒ throw new RESyntaxException("unrecognized flag '" + c + "'")
    }
  }

  for (i ← semi + 1 until flags.length) {
    val c = flags.charAt(i)
    c match {
      case 'g' ⇒ debug = true
    }
  }

  if (language == "XSD") {
  } else if (language == "XP20") {
    xpath20 = true
    if (isLiteral) {
      throw new RESyntaxException("'q' flag requires XPath 3.0 to be enabled")
    }
  } else if (language == "XP30") {
    xpath20 = true
    xpath30 = true
  }

  def isAllowsXPath20Extensions(): Boolean = xpath20

  def isAllowsXPath30Extensions(): Boolean = xpath30
}

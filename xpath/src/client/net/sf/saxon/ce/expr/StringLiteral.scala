// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.value.StringValue

/**
 * Subclass of Literal used specifically for string literals, as this is a common case
 */
class StringLiteral(value: StringValue) extends Literal(value) {

  /**
   * Create a StringLiteral that wraps any CharSequence (including, of course, a String)
   * @param value the CharSequence to be wrapped
   */
  def this(value: CharSequence) {
    this(StringValue.makeStringValue(value))
  }

  /**
   * Get the string represented by this StringLiteral
   * @return the underlying string
   */
  def getStringValue(): String = {
    getValue.asInstanceOf[StringValue].getStringValue
  }

  private def copy(): Expression = {
    new StringLiteral(getValue.asInstanceOf[StringValue])
  }
}

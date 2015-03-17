// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

/**
 *  The expression is generated when compiling the current() function in XSLT. It differs from
 *  the ContextItemExpression "." only in the error code that is returned when there is no context item.
 */
class CurrentItemExpression extends ContextItemExpression {

  /**
   * Get the error code for use when there is no context item
   * @return the string "XTDE1360"
   */
  override protected def getErrorCodeForUndefinedContext(): String = "XTDE1360"
}

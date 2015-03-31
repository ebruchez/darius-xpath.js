// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

/**
 * A NamespaceException represents an error condition whereby a QName (for example a variable
 * name or template name) uses a namespace prefix that is not declared
 */
class NamespaceException(var prefix: String) extends Exception {

  override def getMessage(): String = {
    "Namespace prefix " + prefix + " has not been declared"
  }

  def getPrefix(): String = prefix
}

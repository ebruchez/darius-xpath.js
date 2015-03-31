// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

/**
 * Exception thrown to indicate a syntax error in a regular expression.
 * This is a non-checked exception because you should only have problems compiling
 * a regular expression during development.
 * If you are making regular expresion programs dynamically then you can catch it
 * if you wish. But should not be forced to.
 *
 * @author <a href="mailto:jonl@muppetlabs.com">Jonathan Locke</a>
 * @author <a href="mailto:gholam@xtra.co.nz>Michael McCallum</a>
 * @version $Id: RESyntaxException.java 518156 2007-03-14 14:31:26Z vgritsenko $
 */
class RESyntaxException(s: String) extends RuntimeException("Syntax error in regular expression: " + s) {

  /**
   * Constructor.
   * @param s Further description of the syntax error
   * @param offset character position within regex where the error was detected
   */
  def this(s: String, offset: Int) {
    this("Syntax error at char " + offset + " in regular expression: " + s)
  }
}

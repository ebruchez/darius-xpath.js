// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * xsl:fallback element in stylesheet. <br>
 */
class XSLFallback extends StyleElement {

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  /**
   * Ask whether variables declared in an "uncle" element are visible.
   * @return true for all elements except xsl:fallback and saxon:catch
   */
  protected def seesAvuncularVariables(): Boolean = false

  def prepareAttributes(): Unit = {
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}

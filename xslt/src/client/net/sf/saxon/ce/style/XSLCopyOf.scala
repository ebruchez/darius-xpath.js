// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.CopyOf
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:copy-of element in the stylesheet. <br>
 */
class XSLCopyOf extends StyleElement {

  private var select: Expression = _

  private var copyNamespaces: Boolean = _

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  def prepareAttributes() {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    val b = checkAttribute("copy-namespaces", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      copyNamespaces = b
    }
    checkAttribute("type", "t")
    checkAttribute("validation", "v")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    checkEmpty()
    select = typeCheck(select)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val inst = new CopyOf(select, copyNamespaces)
    inst.setStaticBaseUri(getBaseURI)
    inst
  }
}

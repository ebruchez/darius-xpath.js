// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.DocumentInstr
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:document instruction in the stylesheet. <BR>
 * This instruction creates a document node in the result tree, optionally
 * validating it.
 */
class XSLDocument extends StyleElement {

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

  def prepareAttributes(): Unit = {
    checkAttribute("validation", "v")
    checkAttribute("type", "t")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val inst = new DocumentInstr(false, null, getBaseURI)
    var b = compileSequenceConstructor(exec, decl)
    if (b == null) {
      b = Literal.makeEmptySequence()
    }
    inst.setContentExpression(b)
    inst
  }
}

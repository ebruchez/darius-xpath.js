// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:copy elements in stylesheet. <br>
 */
class XSLCopy extends StyleElement {

  private var use: String = _

  private var attributeSets: Array[AttributeSet] = null

  private var copyNamespaces: Boolean = true

  private var inheritNamespaces: Boolean = true

  private var select: Expression = null

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
    var b = checkAttribute("copy-namespaces", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      copyNamespaces = b
    }
    b = checkAttribute("inherit-namespaces", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      inheritNamespaces = b
    }
    checkAttribute("type", "t")
    checkAttribute("validation", "v")
    use = checkAttribute("use-attribute-sets", "w").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    if (use != null) {
      attributeSets = getAttributeSets(use, null)
    }
    if (select == null) {
      select = new ContextItemExpression()
      select.setSourceLocator(this)
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    select = typeCheck(select)
    try {
      val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:copy/select", 0)
      role.setErrorCode("XTTE2170")
      select = TypeChecker.staticTypeCheck(select, SequenceType.OPTIONAL_ITEM, false, role)
    } catch {
      case err: XPathException ⇒ compileError(err)
    }
    val inst = new Copy(select, copyNamespaces, inheritNamespaces)
    var content = compileSequenceConstructor(exec, decl)
    if (attributeSets != null) {
      val use = new UseAttributeSets(attributeSets)
      val condition = new InstanceOfExpression(new ContextItemExpression(), SequenceType.makeSequenceType(NodeKindTest.ELEMENT, 
        StaticProperty.EXACTLY_ONE))
      val choice = Choose.makeConditional(condition, use)
      if (content == null) {
        content = choice
      } else {
        content = Block.makeBlock(choice, content)
        content.setSourceLocator(this)
      }
    }
    if (content == null) {
      content = Literal.makeEmptySequence()
    }
    inst.setContentExpression(content)
    inst
  }
}

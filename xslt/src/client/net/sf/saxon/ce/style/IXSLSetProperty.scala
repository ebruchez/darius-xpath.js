package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.SetProperty
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

class IXSLSetProperty extends StyleElement {

  private var targetObject: Expression = null

  private var select: Expression = null

  private var name: Expression = null

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = false

  protected override def prepareAttributes() {
    targetObject = checkAttribute("object", "e").asInstanceOf[Expression]
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    name = checkAttribute("name", "a").asInstanceOf[Expression]
    checkForUnknownAttributes()
    if (targetObject == null) {
      targetObject = new IXSLFunction("window", Array.ofDim[Expression](0))
    }
  }

  override def compile(exec: Executable, decl: Declaration): Expression = {
    new SetProperty(targetObject, select, name)
  }
}

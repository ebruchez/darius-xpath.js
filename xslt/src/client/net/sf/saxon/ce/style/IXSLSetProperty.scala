// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.SetProperty
import org.orbeon.darius.xpath.js.IXSLFunction
import org.orbeon.darius.xpath.trans.XPathException
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

  protected override def prepareAttributes(): Unit = {
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

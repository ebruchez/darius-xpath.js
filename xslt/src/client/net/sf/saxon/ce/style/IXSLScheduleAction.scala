// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.ExpressionVisitor
import org.orbeon.darius.xpath.expr.RoleLocator
import org.orbeon.darius.xpath.expr.TypeChecker
import org.orbeon.darius.xpath.expr.instruct.CallTemplate
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.ScheduleExecution
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.linked.NodeImpl
import org.orbeon.darius.xpath.value.SequenceType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Extension element for Saxon client edition: ixsl:schedule-action. The content of the element
 * is currently restricted to a single xsl:call-template instruction
 */
class IXSLScheduleAction extends StyleElement {

  var wait: Expression = _

  var href: Expression = _

  var instruction: XSLCallTemplate = _

  def isInstruction(): Boolean = true

  def prepareAttributes(): Unit = {
    wait = checkAttribute("wait", "e").asInstanceOf[Expression]
    href = checkAttribute("href", "a").asInstanceOf[Expression]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    val visitor = makeExpressionVisitor()
    if (wait != null) {
      wait = typeCheck(wait)
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "ixsl:schedule-action/wait", 0)
        wait = TypeChecker.staticTypeCheck(wait, SequenceType.SINGLE_INTEGER, backwardsCompatible = false, role)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
    }
    var found = false
    for (child ← allChildren()) {
      if (child.isInstanceOf[XSLFallback]) {
      } else if (child.isInstanceOf[XSLCallTemplate]) {
        if (found) {
          compileError("ixsl:schedule-action must contain a single xsl:call-template instruction")
        }
        found = true
        instruction = child.asInstanceOf[XSLCallTemplate]
      }
    }
    if (!found) {
      compileError("ixsl:schedule-action must contain a single xsl:call-template instruction")
    }
  }

  override def compile(exec: Executable, decl: Declaration): Expression = {
    val call = instruction.compile(exec, decl).asInstanceOf[CallTemplate]
    call.setUseTailRecursion(true)
    new ScheduleExecution(call, wait, href)
  }
}

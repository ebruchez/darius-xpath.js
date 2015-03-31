// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.RoleLocator
import client.net.sf.saxon.ce.expr.TypeChecker
import client.net.sf.saxon.ce.expr.instruct.CallTemplate
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.ScheduleExecution
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.value.SequenceType
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
        wait = TypeChecker.staticTypeCheck(wait, SequenceType.SINGLE_INTEGER, false, role)
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

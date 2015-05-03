// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.LogController
import org.orbeon.darius.xpath.expr.AxisExpression
import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.RoleLocator
import org.orbeon.darius.xpath.expr.TypeChecker
import org.orbeon.darius.xpath.expr.instruct.ApplyTemplates
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.sort.SortExpression
import org.orbeon.darius.xpath.expr.sort.SortKeyDefinition
import org.orbeon.darius.xpath.om.Axis
import org.orbeon.darius.xpath.om.NamespaceException
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.Err
import org.orbeon.darius.xpath.trans.Mode
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.SequenceType
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:apply-templates element in the stylesheet
 */
class XSLApplyTemplates extends StyleElement {

  private var select: Expression = _

  private var modeName: StructuredQName = _

  private var useCurrentMode: Boolean = false

  private var useTailRecursion: Boolean = false

  private var defaultedSelectExpression: Boolean = true

  private var mode: Mode = _

  private var modeAttribute: String = _

  private var selectAtt: String = null

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  def prepareAttributes(): Unit = {
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    modeAttribute = checkAttribute("mode", "w").asInstanceOf[String]
    checkForUnknownAttributes()
    if (modeAttribute != null) {
      if (modeAttribute == "#current") {
        useCurrentMode = true
      } else if (modeAttribute == "#default") {
      } else {
        try {
          modeName = makeQName(modeAttribute)
        } catch {
          case err: NamespaceException ⇒
            compileError(err.getMessage, "XTSE0280")
            modeName = null
          case err: XPathException ⇒
            compileError("Mode name " + Err.wrap(modeAttribute) + " is not a valid QName", err.getErrorCodeQName)
            modeName = null
        }
      }
    }
    if (select != null) {
      defaultedSelectExpression = false
    }
  }

  def validate(decl: Declaration): Unit = {
    if (!useCurrentMode) {
      if (modeName == null) {
        modeName = getContainingStylesheet.getDefaultMode
      }
      mode = getPreparedStylesheet.getRuleManager.getMode(modeName, true)
    }
    onlyAllow("sort", "with-param")
    if (select == null) {
      select = new AxisExpression(Axis.CHILD, null)
      select.setSourceLocator(this)
    }
    select = typeCheck(select)
    try {
      val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:apply-templates/select", 0)
      role.setErrorCode("XTTE0520")
      select = TypeChecker.staticTypeCheck(select, SequenceType.NODE_SEQUENCE, backwardsCompatible = false, role)
    } catch {
      case err: XPathException ⇒ compileError(err)
    }
  }

  /**
   * Mark tail-recursive calls on templates and functions.
   * For most instructions, this does nothing.
   */
  def markTailCalls(): Boolean = {
    useTailRecursion = true
    true
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val sortKeys = makeSortKeys(decl)
    if (sortKeys != null) {
      useTailRecursion = false
    }
    var sortedSequence = select
    if (sortKeys != null) {
      sortedSequence = new SortExpression(select, sortKeys)
    }
    compileSequenceConstructor(exec, decl)
    val app = new ApplyTemplates(sortedSequence, useCurrentMode, useTailRecursion, defaultedSelectExpression, 
      mode)
    app.setActualParameters(getWithParamInstructions(exec, decl, false, app), getWithParamInstructions(exec, 
      decl, true, app))
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      if (selectAtt != null) {
        app.AddTraceProperty("select", selectAtt)
      }
      if (modeAttribute != null) {
        app.AddTraceProperty("mode", modeAttribute)
      }
    }
    app
  }
}

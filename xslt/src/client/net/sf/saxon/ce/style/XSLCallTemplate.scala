// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.LogController
import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.CallTemplate
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.Template
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.Err
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.linked.NodeImpl
import org.orbeon.darius.xpath.`type`.AnyItemType
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.value.SequenceType
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:call-template element in the stylesheet
 */
class XSLCallTemplate extends StyleElement {

  private var calledTemplateName: StructuredQName = _

  private var template: XSLTemplate = null

  private var useTailRecursion: Boolean = false

  /**
   * Determine whether this node is an instruction.
   *
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  private var gettingReturnedItemType: Boolean = false

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   *
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = {
    if (template == null || gettingReturnedItemType) {
      AnyItemType.getInstance
    } else {
      gettingReturnedItemType = true
      val result = template.getReturnedItemType
      gettingReturnedItemType = false
      result
    }
  }

  def prepareAttributes(): Unit = {
    calledTemplateName = checkAttribute("name", "q1").asInstanceOf[StructuredQName]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    onlyAllow("with-param")
    if (!(calledTemplateName.getNamespaceURI == NamespaceConstant.SAXON && 
      calledTemplateName.getLocalName == "error-template")) {
      template = findTemplate(calledTemplateName)
    }
  }

  def postValidate(): Unit = {
    if (template != null) {
      for (param ← template.allChildren() if param.isInstanceOf[XSLParam] && param.asInstanceOf[XSLParam].isRequiredParam &&
        !param.asInstanceOf[XSLParam].isTunnelParam) {
        var ok = false
        for (child ← allChildren() if child.isInstanceOf[XSLWithParam] &&
          child.asInstanceOf[XSLWithParam].getVariableQName == param.asInstanceOf[XSLParam].getVariableQName) {
          ok = true
          //break
        }
        if (!ok) {
          compileError("No value supplied for required parameter " + 
            Err.wrap(param.asInstanceOf[XSLParam].getVariableDisplayName, Err.VARIABLE), "XTSE0690")
        }
      }
      for (w ← allChildren() if w.isInstanceOf[XSLWithParam] && !w.asInstanceOf[XSLWithParam].isTunnelParam) {
        val withParam = w.asInstanceOf[XSLWithParam]
        var ok = false
        for (param ← template.allChildren() if param.isInstanceOf[XSLParam] &&
          param.asInstanceOf[XSLParam].getVariableQName == withParam.getVariableQName) {
          ok = true
          val required = param.asInstanceOf[XSLParam].getRequiredType
          withParam.checkAgainstRequiredType(required)
          //break
        }
        if (!ok) {
          if (!xPath10ModeIsEnabled()) {
            compileError("Parameter " + withParam.getVariableDisplayName + " is not declared in the called template", 
              "XTSE0680")
          }
        }
      }
    }
  }

  private def findTemplate(templateName: StructuredQName): XSLTemplate = {
    val psm = getPrincipalStylesheetModule
    val template = psm.getNamedTemplate(templateName)
    if (template == null) {
      compileError("No template exists named " + calledTemplateName, "XTSE0650")
    }
    template
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
    var target: Template = null
    if (template == null) {
      return null
    }
    target = template.getCompiledTemplate
    val call = new CallTemplate(target, useTailRecursion)
    call.setActualParameters(getWithParamInstructions(exec, decl, false, call), getWithParamInstructions(exec, 
      decl, true, call))
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      call.AddTraceProperty("name", calledTemplateName.getDisplayName)
    }
    call
  }
}

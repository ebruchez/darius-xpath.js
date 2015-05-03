// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.ApplyImports
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:apply-imports element in the stylesheet
 */
class XSLApplyImports extends StyleElement {

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  def prepareAttributes(): Unit = {
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    onlyAllow("with-param")
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val inst = new ApplyImports(ApplyImports.APPLY_IMPORTS)
    inst.setActualParameters(getWithParamInstructions(exec, decl, false, inst), getWithParamInstructions(exec, 
      decl, true, inst))
    inst
  }
}

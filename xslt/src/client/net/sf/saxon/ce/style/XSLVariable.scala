// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.GeneralVariable
import client.net.sf.saxon.ce.expr.instruct.GlobalVariable
import client.net.sf.saxon.ce.expr.instruct.LocalVariable
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.SequenceType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:variable elements in stylesheet. <br>
 * The xsl:variable element has mandatory attribute name and optional attribute select
 */
class XSLVariable extends XSLVariableDeclaration {

  private var state: Int = 0

  def prepareAttributes(): Unit = {
    if (state == 2) return
    if (state == 1) {
      compileError("Circular reference to variable", "XTDE0640")
    }
    state = 1
    super.prepareAttributes()
    state = 2
  }

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction (well, it can be, anyway)
   */
  def isInstruction(): Boolean = true

  /**
   * Get the static type of the variable. This is the declared type, unless the value
   * is statically known and constant, in which case it is the actual type of the value.
   */
  def getRequiredType(): SequenceType = {
    val th = TypeHierarchy.getInstance
    val defaultType = (if (requiredType == null) SequenceType.ANY_SEQUENCE else requiredType)
    if (requiredType != null) {
      requiredType
    } else if (select != null) {
      if (Literal.isEmptySequence(select)) {
        defaultType
      } else {
        try {
          SequenceType.makeSequenceType(select.getItemType, select.getCardinality)
        } catch {
          case err: Exception => defaultType
        }
      }
    } else if (hasChildNodes()) {
      SequenceType.makeSequenceType(NodeKindTest.DOCUMENT, StaticProperty.EXACTLY_ONE)
    } else {
      SequenceType.SINGLE_STRING
    }
  }

  /**
   * Compile: used only for global variables.
   * This method ensures space is available for local variables declared within
   * this global variable
   */
  def compile(exec: Executable, decl: Declaration): Expression = {
    if (references.isEmpty) {
      redundant = true
    }
    if (!redundant) {
      var inst: GeneralVariable = null
      if (global) {
        inst = new GlobalVariable()
        inst.asInstanceOf[GlobalVariable].setExecutable(getExecutable)
        if (select != null) {
          select.setContainer(inst.asInstanceOf[GlobalVariable])
        }
        initializeInstruction(exec, decl, inst)
        inst.setVariableQName(getVariableQName)
        inst.setSlotNumber(getSlotNumber)
        inst.setRequiredType(getRequiredType)
        fixupBinding(inst)
        inst.setContainer(inst.asInstanceOf[GlobalVariable])
        compiledVariable = inst
        return inst
      } else {
        throw new AssertionError("Local variable found when compiling a global variable")
      }
    }
    null
  }

  def compileLocalVariable(exec: Executable, decl: Declaration): Expression = {
    if (references.isEmpty) {
      redundant = true
    }
    if (!redundant) {
      var inst: GeneralVariable = null
      if (global) {
        throw new AssertionError("Global variable found when compiling local variable")
      } else {
        inst = new LocalVariable()
        inst.setContainer(this)
        if (select != null) {
          select.setContainer(this)
        }
        initializeInstruction(exec, decl, inst)
        inst.setVariableQName(getVariableQName)
        inst.setRequiredType(getRequiredType)
        return inst
      }
    }
    null
  }
}

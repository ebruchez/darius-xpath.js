// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.GlobalVariable
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.SequenceType
import java.util.ArrayList
import java.util.List
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Generic class for xsl:variable and xsl:param elements. <br>
 */
abstract class XSLVariableDeclaration extends XSLGeneralVariable with VariableDeclaration with StylesheetProcedure {

  @BeanProperty
  var slotNumber: Int = -9876

  protected var references: List[VariableReference] = new ArrayList[VariableReference](10)

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  /**
   * Get the static type of the variable.
   * @return the static type declared for the variable
   */
  def getRequiredType(): SequenceType

  /**
   * Method called by VariableReference to register the variable reference for
   * subsequent fixup
   */
  def registerReference(ref: VariableReference): Unit = {
    references.add(ref)
  }

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction (well, it can be, anyway)
   */
  def isInstruction(): Boolean = true

  /**
   * Notify all references to this variable of the data type
   */
  def fixupReferences(): Unit = {
    val `type` = getRequiredType
    val th = TypeHierarchy.getInstance
    for (reference <- references) {
      var constantValue: Sequence = null
      var properties = 0
      if (this.isInstanceOf[XSLVariable]) {
        if (select.isInstanceOf[Literal]) {
          val relation = th.relationship(select.getItemType, `type`.getPrimaryType)
          if (relation == TypeHierarchy.SAME_TYPE || relation == TypeHierarchy.SUBSUMED_BY) {
            constantValue = select.asInstanceOf[Literal].getValue
          }
        }
        if (select != null) {
          properties = select.getSpecialProperties
        }
      }
      reference.setStaticType(`type`, constantValue, properties)
    }
    super.fixupReferences()
  }

  /**
   * Check that the variable is not already declared, and allocate a slot number
   * @param decl
   */
  def validate(decl: Declaration): Unit = {
    super.validate(decl)
    if (global) {
      if (!redundant) {
        slotNumber = getExecutable.allocateGlobalVariableSlot()
      }
    }
  }

  /**
   * Notify all variable references of the Binding instruction
   * @param binding the Binding that represents this variable declaration in the executable code tree
   */
  protected def fixupBinding(binding: Binding): Unit = {
    for (reference <- references) {
      reference.fixup(binding)
    }
  }

  protected def index(decl: Declaration, top: PrincipalStylesheetModule): Unit = {
    top.indexVariableDeclaration(decl)
  }

  /**
   * Optimize the stylesheet construct
   * @param declaration
   */
  def optimize(declaration: Declaration): Unit = {
    if (global && !redundant && select != null) {
      var exp2 = select
      val visitor = makeExpressionVisitor()
      try {
        exp2 = exp2.optimize(visitor, AnyNodeTest.getInstance)
      } catch {
        case err: XPathException => {
          err.maybeSetLocation(this)
          compileError(err)
        }
      }
      val numberOfSlots = ExpressionTool.allocateSlots(exp2, 0)
      compiledVariable.asInstanceOf[GlobalVariable].setContainsLocals(numberOfSlots)
      if (exp2 != select) {
        select = exp2
        compiledVariable.setSelectExpression(select)
      }
    }
  }
}

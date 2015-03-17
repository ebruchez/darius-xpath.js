package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.RoleLocator
import client.net.sf.saxon.ce.expr.SuppliedParameterReference
import client.net.sf.saxon.ce.expr.TypeChecker
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.Whitespace
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:param element in the stylesheet. <br>
 * The xsl:param element has mandatory attribute name and optional attributes
 *  select, required, as, ...
 */
class XSLParam extends XSLVariableDeclaration {

  var conversion: Expression = null

  protected def allowsValue(): Boolean = !(getParent.isInstanceOf[XSLFunction])

  protected def allowsRequired(): Boolean = {
    getParent.asInstanceOf[StyleElement].mayContainParam("required")
  }

  protected def allowsTunnelAttribute(): Boolean = true

  def validate(decl: Declaration) {
    val parent = getParent
    global = (parent.isInstanceOf[XSLStylesheet])
    if (!((parent.isInstanceOf[StyleElement]) && 
      parent.asInstanceOf[StyleElement].mayContainParam(null))) {
      compileError("xsl:param must be immediately within a template, function or stylesheet", "XTSE0010")
    }
    if (!global) {
      val prec = iterateAxis(Axis.PRECEDING_SIBLING, AnyNodeTest.getInstance)
      var index = 0
      var node: NodeImpl = null
      while (true) {
        node = prec.next().asInstanceOf[NodeImpl]
        if (node == null) {
          //break
        }
        if (node.isInstanceOf[XSLParam]) {
          if (this.getVariableQName == node.asInstanceOf[XSLParam].getVariableQName) {
            compileError("The name of the parameter is not unique", "XTSE0580")
          }
          index += 1
        } else if (node.isInstanceOf[StyleElement]) {
          compileError("xsl:param must not be preceded by other instructions", "XTSE0010")
        } else {
          if (!Whitespace.isWhite(node.getStringValue)) {
            compileError("xsl:param must not be preceded by text", "XTSE0010")
          }
        }
      }
      setSlotNumber(index)
    }
    if (requiredParam) {
      if (select != null) {
        compileError("The select attribute should be omitted when required='yes'", "XTSE0010")
      }
      if (hasChildNodes()) {
        compileError("A parameter specifying required='yes' must have empty content", "XTSE0010")
      }
    }
    super.validate(decl)
  }

  /**
   * Compile: this ensures space is available for local variables declared within
   * this global variable
   */
  def compile(exec: Executable, decl: Declaration): Expression = {
    if (redundant) {
      return null
    }
    if (getParent.isInstanceOf[XSLFunction]) {
      null
    } else {
      val slot = getSlotNumber
      if (requiredType != null) {
        val pref = new SuppliedParameterReference(slot)
        pref.setSourceLocator(this)
        val role = new RoleLocator(RoleLocator.PARAM, getVariableDisplayName, 0)
        role.setErrorCode("XTTE0590")
        conversion = TypeChecker.staticTypeCheck(pref, requiredType, false, role)
      }
      var inst: GeneralVariable = null
      if (global) {
        inst = new GlobalParam()
        inst.asInstanceOf[GlobalParam].setExecutable(getExecutable)
        inst.setContainer(inst.asInstanceOf[GlobalParam])
        if (isRequiredParam) {
          getExecutable.addRequiredParam(getVariableQName)
        }
        if (select != null) {
          select.setContainer(inst.asInstanceOf[GlobalVariable])
        }
        compiledVariable = inst
      } else {
        val psm = getPrincipalStylesheetModule
        inst = new LocalParam()
        inst.asInstanceOf[LocalParam].setConversion(conversion)
        inst.asInstanceOf[LocalParam].setParameterId(psm.allocateUniqueParameterNumber(getVariableQName))
      }
      initializeInstruction(exec, decl, inst)
      inst.setVariableQName(getVariableQName)
      inst.setSlotNumber(slot)
      inst.setRequiredType(getRequiredType)
      fixupBinding(inst)
      compiledVariable = inst
      inst
    }
  }

  /**
   * Get the static type of the parameter. This is the declared type, because we cannot know
   * the actual value in advance.
   */
  def getRequiredType(): SequenceType = {
    if (requiredType != null) {
      requiredType
    } else {
      SequenceType.ANY_SEQUENCE
    }
  }
}

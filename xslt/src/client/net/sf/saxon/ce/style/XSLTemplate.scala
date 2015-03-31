// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.Template
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.NamespaceException
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.trans.Mode
import client.net.sf.saxon.ce.trans.RuleManager
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.DecimalValue
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.logging.client.LogConfiguration
import java.util.List
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:template element in the style sheet.
 */
class XSLTemplate extends StyleElement with StylesheetProcedure {

  private var priorityAtt: String = null

  private var prepared: Boolean = false

  private var templateName: StructuredQName = _

  private var modeNames: Array[StructuredQName] = _

  private var `match`: Pattern = _

  private var prioritySpecified: Boolean = _

  private var priority: Double = _

  @BeanProperty
  var compiledTemplate: Template = new Template()

  private var requiredType: SequenceType = null

  private var numberOfParams: Int = _

  private var hasRequiredParams: Boolean = false

  private var ixslPreventDefault: Boolean = false

  private var ixslEventProperty: String = null

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  protected def mayContainParam(attName: String): Boolean = true

  /**
   * Specify that xsl:param is a permitted child
   */
  protected def isPermittedChild(child: StyleElement): Boolean = child.isInstanceOf[XSLParam]

  /**
   * Return the name of this template. Note that this may
   * be called before prepareAttributes has been called.
   * @return the name of the template as a Structured QName.
   * Returns null for an unnamed template (unlike getObjectName(),
   * which returns an invented name)
   */
  def getTemplateName(): StructuredQName = {
    if (templateName == null && !prepared) {
      try {
        prepareAttributes()
      } catch {
        case err: XPathException => 
      }
    }
    templateName
  }

  /**
   * Determine the type of item returned by this template
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = {
    if (requiredType == null) {
      getCommonChildItemType
    } else {
      requiredType.getPrimaryType
    }
  }

  def prepareAttributes(): Unit = {
    if (prepared) {
      return
    }
    prepared = true
    templateName = checkAttribute("name", "q").asInstanceOf[StructuredQName]
    if (templateName != null) {
      setObjectName(templateName)
    }
    val modeAtt = checkAttribute("mode", "s").asInstanceOf[String]
    `match` = checkAttribute("match", "p").asInstanceOf[Pattern]
    priorityAtt = checkAttribute("priority", "w").asInstanceOf[String]
    requiredType = checkAttribute("as", "z").asInstanceOf[SequenceType]
    checkForUnknownAttributes()
    val a = getAttributeValue(NamespaceConstant.IXSL, "prevent-default")
    ixslPreventDefault = "yes" == a
    ixslEventProperty = getAttributeValue(NamespaceConstant.IXSL, "event-property")
    if (`match` == null) {
      if (templateName == null) {
        compileError("A template must have a name or match pattern (or both)", "XTSE0500")
      }
      if (modeAtt != null || priorityAtt != null) {
        compileError("A template with no match pattern must have no mode or priority", "XTSE0500")
      }
    }
    try {
      if (modeAtt != null) {
        val tokens = Whitespace.tokenize(modeAtt)
        var count = 0
        if (tokens.size == 0) {
          compileError("The mode attribute must not be empty", "XTSE0550")
        }
        modeNames = Array.ofDim[StructuredQName](tokens.size)
        count = 0
        var allModes = false
        for (s <- tokens) {
          var mname: StructuredQName = null
          if ("#default" == s) {
            mname = getContainingStylesheet.getDefaultMode
            if (mname == null) {
              mname = Mode.UNNAMED_MODE_NAME
            }
          } else if ("#all" == s) {
            allModes = true
            mname = Mode.ALL_MODES
          } else {
            mname = makeQName(s)
          }
          for (e <- 0 until count if modeNames(e) == mname) {
            compileError("In the list of modes, the value " + s + " is duplicated", "XTSE0550")
          }
          modeNames(count += 1) = mname
        }
        if (allModes && (count > 1)) {
          compileError("mode='#all' cannot be combined with other modes", "XTSE0550")
        }
      } else {
        modeNames = Array(Mode.UNNAMED_MODE_NAME)
      }
    } catch {
      case err: NamespaceException => compileError(err.getMessage, "XTSE0280")
      case err: XPathException => {
        err.maybeSetErrorCode("XTSE0280")
        if (err.getErrorCodeLocalPart == "XTSE0020") {
          err.setErrorCode("XTSE0550")
        }
        err.setIsStaticError(true)
        compileError(err)
      }
    }
    prioritySpecified = priorityAtt != null
    if (prioritySpecified) {
      try {
        if (!DecimalValue.castableAsDecimal(priorityAtt)) {
          compileError("Invalid numeric value for priority (" + priority + ')', "XTSE0530")
        }
        priority = Double.parseDouble(priorityAtt)
      } catch {
        case err: NumberFormatException => priority = -1e0
      }
    }
  }

  def validate(decl: Declaration): Unit = {
    checkTopLevel(null)
    `match` = typeCheck("match", `match`)
    numberOfParams = 0
    for (param <- allChildren() if param.isInstanceOf[XSLParam]) {
      numberOfParams += 1
      if (param.asInstanceOf[XSLParam].isRequiredParam) {
        hasRequiredParams = true
      }
    }
  }

  def postValidate(): Unit = {
    markTailCalls()
  }

  protected def index(decl: Declaration, top: PrincipalStylesheetModule): Unit = {
    top.indexNamedTemplate(decl)
  }

  /**
   * Mark tail-recursive calls on templates and functions.
   */
  def markTailCalls(): Boolean = {
    val last = getLastChildInstruction
    last != null && last.markTailCalls()
  }

  /**
   * Compile: creates the executable form of the template
   */
  def compile(exec: Executable, decl: Declaration): Expression = {
    var block = compileSequenceConstructor(exec, decl)
    if (block == null) {
      block = Literal.makeEmptySequence()
    }
    compiledTemplate.setMatchPattern(`match`)
    compiledTemplate.setBody(block)
    compiledTemplate.setExecutable(getExecutable)
    compiledTemplate.setSourceLocator(this)
    compiledTemplate.setHasRequiredParams(hasRequiredParams)
    compiledTemplate.setRequiredType(requiredType)
    var exp: Expression = null
    try {
      exp = makeExpressionVisitor().simplify(block)
    } catch {
      case e: XPathException => compileError(e)
    }
    try {
      if (requiredType != null) {
        val role = new RoleLocator(RoleLocator.TEMPLATE_RESULT, getDiagnosticId, 0)
        role.setErrorCode("XTTE0505")
        exp = TypeChecker.staticTypeCheck(exp, requiredType, false, role)
      }
    } catch {
      case err: XPathException => compileError(err)
    }
    compiledTemplate.setBody(exp)
    compiledTemplate.setTemplateName(getObjectName)
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      exp = makeTraceInstruction(this, exp)
      if (exp.isInstanceOf[TraceExpression]) {
        exp.asInstanceOf[TraceExpression].setProperty("match", `match`.toString)
        exp.asInstanceOf[TraceExpression].setProperty("mode", getAttributeValue("", "mode"))
      }
      compiledTemplate.setBody(exp)
    }
    null
  }

  /**
   * Returns a string that identifies the template for diagnostics
   * @return an identifying string
   */
  def getDiagnosticId(): String = {
    if (templateName != null) {
      templateName.getDisplayName
    } else {
      `match`.toString
    }
  }

  /**
   * Registers the template rule with each Mode that it belongs to.
   * @param declaration Associates this template with a stylesheet module (in principle an xsl:template
   * element can be in a document that is imported more than once; these are separate declarations)
   * @throws XPathException
   */
  def register(declaration: Declaration): Unit = {
    if (`match` != null) {
      val module = declaration.getModule
      val slots = `match`.allocateSlots(0)
      val mgr = getPreparedStylesheet.getRuleManager
      for (nc <- modeNames) {
        val mode = mgr.getMode(nc, true)
        if (prioritySpecified) {
          mgr.setTemplateRule(`match`, compiledTemplate, mode, module, priority, ixslPreventDefault, 
            ixslEventProperty)
        } else {
          mgr.setTemplateRule(`match`, compiledTemplate, mode, module, Double.NaN, ixslPreventDefault, 
            ixslEventProperty)
        }
        mode.allocatePatternSlots(slots)
      }
      allocatePatternSlots(slots)
    }
  }

  /**
   * This method is a bit of a misnomer, because it does more than invoke optimization of the template body.
   * In particular, it also registers the template rule with each Mode that it belongs to.
   * @throws XPathException
   * @param declaration Associates this template with a stylesheet module (in principle an xsl:template
   * element can be in a document that is imported more than once; these are separate declarations)
   */
  def optimize(declaration: Declaration): Unit = {
    var contextItemType = Type.ITEM_TYPE
    if (getObjectName == null) {
      contextItemType = `match`.getNodeTest
    }
    val exp = compiledTemplate.getBody
    val visitor = makeExpressionVisitor()
    try {
      var exp2 = visitor.typeCheck(exp, contextItemType)
      exp2 = visitor.optimize(exp2, contextItemType)
      if (exp != exp2) {
        compiledTemplate.setBody(exp2)
      }
    } catch {
      case e: XPathException => compileError(e)
    }
    compiledTemplate.allocateSlots(numberOfParams)
  }
}

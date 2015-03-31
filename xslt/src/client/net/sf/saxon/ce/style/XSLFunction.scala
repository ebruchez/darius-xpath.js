// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.UserFunction
import client.net.sf.saxon.ce.expr.instruct.UserFunctionParameter
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.value.SequenceType
import com.google.gwt.logging.client.LogConfiguration
import java.util.ArrayList
import java.util.List
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:function elements in stylesheet (XSLT 2.0). <BR>
 * Attributes: <br>
 * name gives the name of the function
 */
class XSLFunction extends StyleElement with StylesheetProcedure {

  private var prepared: Boolean = false

  @BeanProperty
  var resultType: SequenceType = _

  private var override: Boolean = true

  private var numberOfArguments: Int = -1

  @BeanProperty
  var compiledFunction: UserFunction = _

  var references: List[UserFunctionCall] = new ArrayList[UserFunctionCall](10)

  /**
   * Method called by UserFunctionCall to register the function call for
   * subsequent fixup.
   * @param ref the UserFunctionCall to be registered
   */
  def registerReference(ref: UserFunctionCall): Unit = {
    references.add(ref)
  }

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes(): Unit = {
    if (prepared) {
      return
    }
    prepared = true
    setObjectName(checkAttribute("name", "q1").asInstanceOf[StructuredQName])
    resultType = checkAttribute("as", "z").asInstanceOf[SequenceType]
    val b = checkAttribute("override", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      override = b
    }
    checkForUnknownAttributes()
    if (resultType == null) {
      resultType = SequenceType.ANY_SEQUENCE
    }
    if (getObjectName.getNamespaceURI == "") {
      compileError("Function name must have a namespace prefix", "XTSE0740")
    }
  }

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   * If there is no name, the value will be -1.
   */
  def getObjectName(): StructuredQName = {
    var qn = super.getObjectName
    if (qn == null) {
      try {
        qn = checkAttribute("name", "q1").asInstanceOf[StructuredQName]
      } catch {
        case e: XPathException ⇒
      }
      setObjectName(qn)
    }
    qn
  }

  /**
   * Determine whether this type of element is allowed to contain a template-body.
   * @return true: yes, it may contain a general template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  protected def mayContainParam(attName: String): Boolean = "required" != attName

  /**
   * Specify that xsl:param is a permitted child
   */
  protected def isPermittedChild(child: StyleElement): Boolean = child.isInstanceOf[XSLParam]

  /**
   * Is override="yes"?.
   * @return true if override="yes" was specified, otherwise false
   */
  def isOverriding(): Boolean = {
    if (!prepared) {
      try {
        val b = checkAttribute("override", "b").asInstanceOf[java.lang.Boolean]
        if (b != null) {
          override = b
        }
      } catch {
        case e: XPathException ⇒
      }
    }
    override
  }

  protected def index(decl: Declaration, top: PrincipalStylesheetModule): Unit = {
    top.indexFunction(decl)
  }

  /**
   * Notify all references to this function of the data type.
   * @throws XPathException
   */
  def fixupReferences(): Unit = {
    for (reference ← references) {
      reference.setStaticType(resultType)
    }
    super.fixupReferences()
  }

  def validate(decl: Declaration): Unit = {
    checkTopLevel(null)
    getNumberOfArguments
  }

  /**
   * Compile the function definition to create an executable representation
   * @return an Instruction, or null. The instruction returned is actually
   * rather irrelevant; the compile() method has the side-effect of binding
   * all references to the function to the executable representation
   * (a UserFunction object)
   * @throws XPathException
   */
  def compile(exec: Executable, decl: Declaration): Expression = {
    compileAsExpression(exec, decl)
    null
  }

  /**
   * Compile the function into a UserFunction object, which treats the function
   * body as a single XPath expression. This involves recursively translating
   * xsl:variable declarations into let expressions, withe the action part of the
   * let expression containing the rest of the function body.
   * The UserFunction that is created will be linked from all calls to
   * this function, so nothing else needs to be done with the result. If there are
   * no calls to it, the compiled function will be garbage-collected away.
   * @param exec the Executable
   * @param decl
   * @throws XPathException
   */
  private def compileAsExpression(exec: Executable, decl: Declaration): Unit = {
    var exp = compileSequenceConstructor(exec, decl)
    if (exp == null) {
      exp = Literal.makeEmptySequence()
    } else {
      val visitor = makeExpressionVisitor()
      exp = exp.simplify(visitor)
    }
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      val trace = new TraceExpression(exp)
      trace.setConstructType(getNodeName)
      trace.setObjectName(getObjectName)
      exp = trace
    }
    val fn = new UserFunction()
    fn.setBody(exp)
    fn.setFunctionName(getObjectName)
    setParameterDefinitions(fn)
    fn.setResultType(getResultType)
    fn.setSourceLocator(this)
    fn.setExecutable(exec)
    compiledFunction = fn
    fixupInstruction(fn)
  }

  def typeCheckBody(): Unit = {
    val exp = compiledFunction.getBody
    var exp2 = exp
    val visitor = makeExpressionVisitor()
    try {
      exp2 = visitor.typeCheck(exp, null)
      if (resultType != null) {
        val role = new RoleLocator(RoleLocator.FUNCTION_RESULT, getObjectName.getDisplayName, 0)
        role.setErrorCode("XTTE0780")
        exp2 = TypeChecker.staticTypeCheck(exp2, resultType, false, role)
      }
    } catch {
      case err: XPathException ⇒ {
        err.maybeSetLocation(this)
        compileError(err)
      }
    }
    if (exp2 != exp) {
      compiledFunction.setBody(exp2)
    }
  }

  def optimize(declaration: Declaration): Unit = {
    val exp = compiledFunction.getBody
    val visitor = makeExpressionVisitor()
    var exp2 = exp
    try {
      exp2 = exp.optimize(visitor, null)
    } catch {
      case err: XPathException ⇒ {
        err.maybeSetLocation(this)
        compileError(err)
      }
    }
    if (exp2 != exp) {
      compiledFunction.setBody(exp2)
    }
    val tailCalls = ExpressionTool.markTailFunctionCalls(exp2, getObjectName, getNumberOfArguments)
    if (tailCalls != 0) {
      compiledFunction.setTailRecursive(tailCalls > 0, tailCalls > 1)
      compiledFunction.setBody(new TailCallLoop(compiledFunction))
    }
    compiledFunction.allocateSlots(getNumberOfArguments)
    compiledFunction.computeEvaluationMode()
  }

  /**
   * Fixup all function references.
   * @param compiledFunction the Instruction representing this function in the compiled code
   * @throws XPathException if an error occurs.
   */
  private def fixupInstruction(compiledFunction: UserFunction): Unit = {
    val visitor = makeExpressionVisitor()
    try {
      for (call ← references) {
        call.setFunction(compiledFunction)
        call.checkFunctionCall(compiledFunction, visitor)
      }
    } catch {
      case err: XPathException ⇒ compileError(err)
    }
  }

  /**
   * Get the number of arguments declared by this function (that is, its arity).
   * @return the arity of the function
   */
  def getNumberOfArguments(): Int = {
    if (numberOfArguments == -1) {
      numberOfArguments = 0
      for (child ← allChildren()) {
        if (child.isInstanceOf[XSLParam]) {
          numberOfArguments += 1
        } else {
          return numberOfArguments
        }
      }
    }
    numberOfArguments
  }

  /**
   * Set the definitions of the parameters in the compiled function, as an array.
   * @param fn the compiled object representing the user-written function
   */
  def setParameterDefinitions(fn: UserFunction): Unit = {
    val params = Array.ofDim[UserFunctionParameter](getNumberOfArguments)
    fn.setParameterDefinitions(params)
    val count = 0
    for (child ← allChildren() if child.isInstanceOf[XSLParam]) {
      val param = new UserFunctionParameter()
      params(count += 1) = param
      param.setRequiredType(child.asInstanceOf[XSLParam].getRequiredType)
      param.setVariableQName(child.asInstanceOf[XSLParam].getVariableQName)
      param.setSlotNumber(child.asInstanceOf[XSLParam].getSlotNumber)
      child.asInstanceOf[XSLParam].fixupBinding(param)
    }
  }
}

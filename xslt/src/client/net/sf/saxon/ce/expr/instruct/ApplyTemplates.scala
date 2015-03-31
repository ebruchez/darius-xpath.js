// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.Mode
import client.net.sf.saxon.ce.trans.Rule
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.FocusIterator
import client.net.sf.saxon.ce.tree.util.SourceLocator
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import java.util.ArrayList
import java.util.Iterator
import ApplyTemplates._
//remove if not needed
import scala.collection.JavaConversions._

object ApplyTemplates {

  /**
   * Perform the built-in template action for a given node.
   * @param node         the node to be processed
   * @param parameters   the parameters supplied to apply-templates
   * @param tunnelParams the tunnel parameters to be passed through
   * @param context      the dynamic evaluation context
   * @param sourceLocator   location of the instruction (apply-templates, apply-imports etc) that caused
   *                     the built-in template to be invoked
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any dynamic error occurs
   */
  def defaultAction(node: NodeInfo, 
      parameters: ParameterSet, 
      tunnelParams: ParameterSet, 
      context: XPathContext, 
      sourceLocator: SourceLocator) node.getNodeKind match {
    case Type.DOCUMENT | Type.ELEMENT => 
      var iter = node.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
      var c2 = context.newContext()
      var tc = applyTemplates(iter, context.getCurrentMode, parameters, tunnelParams, c2, sourceLocator)
      while (tc != null) {
        tc = tc.processLeavingTail()
      }
      return

    case Type.TEXT | Type.ATTRIBUTE => 
      context.getReceiver.characters(node.getStringValue)
      return

    case Type.COMMENT | Type.PROCESSING_INSTRUCTION | Type.NAMESPACE}

  /**
   * Process selected nodes using the handlers registered for a particular
   * mode.
   *
   * @exception XPathException if any dynamic error occurs
   * @param iterator an Iterator over the nodes to be processed, in the
   *     correct (sorted) order
   * @param mode Identifies the processing mode. It should match the
   *     mode defined when the element handler was registered using
   *     setHandler with a mode parameter. Set this parameter to null to
   *     invoke the default mode.
   * @param parameters A ParameterSet containing the parameters to
   *     the handler/template being invoked. Specify null if there are no
   *     parameters.
   * @param tunnelParameters A ParameterSet containing the parameters to
   *     the handler/template being invoked. Specify null if there are no
   *     parameters.
   * @param context A newly-created context object (this must be freshly created by the caller,
   * as it will be modified by this method)
   * @param sourceLocator location of this apply-templates instruction in the stylesheet
   * @return a TailCall returned by the last template to be invoked, or null,
   *     indicating that there are no outstanding tail calls.
   */
  def applyTemplates(iterator: SequenceIterator, 
      mode: Mode, 
      parameters: ParameterSet, 
      tunnelParameters: ParameterSet, 
      context: XPathContext, 
      sourceLocator: SourceLocator): TailCall = {
    var tc: TailCall = null
    val focus = context.setCurrentIterator(iterator)
    context.setCurrentMode(mode)
    var previousTemplate: Template = null
    while (true) {
      if (tc != null) {
        do {
          tc = tc.processLeavingTail()
        } while (tc != null);
      }
      val node = focus.next().asInstanceOf[NodeInfo]
      if (node == null) {
        //break
      }
      var rule = mode.getRule(node, context)
      if (rule == null) {
        rule = mode.getVirtualRule(context)
      }
      if (rule == null) {
        defaultAction(node, parameters, tunnelParameters, context, sourceLocator)
      } else {
        val template = rule.getAction
        if (template != previousTemplate) {
          previousTemplate = template
          context.setParameters(template.getNumberOfSlots, parameters, tunnelParameters)
        }
        context.setCurrentTemplateRule(rule)
        if (rule.isVirtual) {
          val iter = IXSLFunction.convertFromJavaScript(context.getController.getUserData("Saxon-CE", 
            "current-object"), context.getConfiguration)
          iter.next()
          context.setCurrentIterator(iter)
        }
        tc = template.applyLeavingTail(context)
      }
    }
    tc
  }

  /**
   * An ApplyTemplatesPackage is an object that encapsulates the sequence of nodes to be processed,
   * the mode, the parameters to be supplied, and the execution context. This object can be returned as a tail
   * call, so that the actual call is made from a lower point on the stack, allowing a tail-recursive
   * template to execute in a finite stack size
   */
  private class ApplyTemplatesPackage(var selectedNodes: Sequence, 
      var mode: Mode, 
      var params: ParameterSet, 
      var tunnelParams: ParameterSet, 
      var evaluationContext: XPathContext, 
      var sourceLocator: SourceLocator) extends TailCall {

    def processLeavingTail(): TailCall = {
      applyTemplates(selectedNodes.iterate(), mode, params, tunnelParams, evaluationContext, sourceLocator)
    }
  }
}

/**
 * An instruction representing an xsl:apply-templates element in the stylesheet
 */
class ApplyTemplates protected () extends Instruction {

  protected var select: Expression = _

  protected var actualParams: Array[WithParam] = null

  protected var tunnelParams: Array[WithParam] = null

  protected var useCurrentMode: Boolean = false

  protected var useTailRecursion: Boolean = false

  protected var mode: Mode = _

  protected var implicitSelect: Boolean = _

  /**
   * Construct an apply-templates instructino
   * @param select the select expression
   * @param useCurrentMode true if mode="#current" was specified
   * @param useTailRecursion true if this instruction is the last in its template
   * @param mode the mode specified on apply-templates
   */
  def this(select: Expression, 
      useCurrentMode: Boolean, 
      useTailRecursion: Boolean, 
      implicitSelect: Boolean, 
      mode: Mode) {
    this()
    init(select, useCurrentMode, useTailRecursion, mode)
    this.implicitSelect = implicitSelect
  }

  protected def init(select: Expression, 
      useCurrentMode: Boolean, 
      useTailRecursion: Boolean, 
      mode: Mode): Unit = {
    this.select = select
    this.useCurrentMode = useCurrentMode
    this.useTailRecursion = useTailRecursion
    this.mode = mode
    adoptChildExpression(select)
  }

  /**
   * Set the actual parameters on the call
   * @param actualParams represents the contained xsl:with-param elements having tunnel="no" (the default)
   * @param tunnelParams represents the contained xsl:with-param elements having tunnel="yes"
   */
  def setActualParameters(actualParams: Array[WithParam], tunnelParams: Array[WithParam]): Unit = {
    this.actualParams = actualParams
    this.tunnelParams = tunnelParams
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @exception XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor the expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    WithParam.simplify(actualParams, visitor)
    WithParam.simplify(tunnelParams, visitor)
    select = visitor.simplify(select)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    WithParam.typeCheck(actualParams, visitor, contextItemType)
    WithParam.typeCheck(tunnelParams, visitor, contextItemType)
    try {
      select = visitor.typeCheck(select, contextItemType)
    } catch {
      case e: XPathException => {
        if (implicitSelect) {
          val code = e.getErrorCodeLocalPart
          if ("XPTY0020" == code) {
            val err = new XPathException("Cannot apply-templates to child nodes when the context item is an atomic value")
            err.setErrorCode("XTTE0510")
            err.setIsTypeError(true)
            throw err
          } else if ("XPDY0002" == code) {
            val err = new XPathException("Cannot apply-templates to child nodes when the context item is undefined")
            err.setErrorCode("XTTE0510")
            err.setIsTypeError(true)
            throw err
          }
        }
        throw e
      }
    }
    adoptChildExpression(select)
    if (Literal.isEmptySequence(select)) {
      return select
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    WithParam.optimize(visitor, actualParams, contextItemType)
    WithParam.optimize(visitor, tunnelParams, contextItemType)
    select = visitor.typeCheck(select, contextItemType)
    select = visitor.optimize(select, contextItemType)
    adoptChildExpression(select)
    if (Literal.isEmptySequence(select)) {
      return select
    }
    this
  }

  def getIntrinsicDependencies(): Int = {
    super.getIntrinsicDependencies | 
      (if (useCurrentMode) StaticProperty.DEPENDS_ON_CURRENT_ITEM else 0)
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true (which is almost invariably the case, so it's not worth
   * doing any further analysis to find out more precisely).
   */
  def createsNewNodes(): Boolean = true

  def process(context: XPathContext): Unit = {
    apply(context, false)
  }

  def processLeavingTail(context: XPathContext): TailCall = apply(context, useTailRecursion)

  protected def apply(context: XPathContext, returnTailCall: Boolean): TailCall = {
    var thisMode = mode
    if (useCurrentMode) {
      thisMode = context.getCurrentMode
    }
    val params = assembleParams(context, actualParams)
    val tunnels = assembleTunnelParams(context, tunnelParams)
    if (returnTailCall) {
      val c2 = context.newContext()
      val evaluationMode = ExpressionTool.lazyEvaluationMode(select)
      return new ApplyTemplatesPackage(ExpressionTool.evaluate(select, evaluationMode, context), thisMode, 
        params, tunnels, c2, getSourceLocator)
    }
    val iter = select.iterate(context)
    if (iter.isInstanceOf[EmptyIterator]) {
      return null
    }
    val c2 = context.newContext()
    var tc = applyTemplates(iter, thisMode, params, tunnels, c2, getSourceLocator)
    while (tc != null) {
      tc = tc.processLeavingTail()
    }
    null
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    val list = new ArrayList[Expression](10)
    list.add(select)
    WithParam.getXPathExpressions(actualParams, list)
    WithParam.getXPathExpressions(tunnelParams, list)
    list.iterator()
  }

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  def hasLoopingSubexpression(child: Expression): Boolean = child.isInstanceOf[WithParam]

  /**
   * Get the select expression
   * @return the select expression
   */
  def getSelectExpression(): Expression = select

  /**
   * Ask if the select expression was implicit
   * @return true if no select attribute was explicitly specified
   */
  def isImplicitSelect(): Boolean = implicitSelect

  /**
   * Ask if tail recursion is to be used
   * @return true if tail recursion is used
   */
  def useTailRecursion(): Boolean = useTailRecursion

  /**
   * Ask if mode="#current" was specified
   * @return true if mode="#current" was specified
   */
  def usesCurrentMode(): Boolean = useCurrentMode

  /**
   * Get the Mode
   * @return the mode, or null if mode="#current" was specified
   */
  def getMode(): Mode = mode

  /**
   * Get the actual parameters passed to the called template
   * @return the non-tunnel parameters
   */
  def getActualParams(): Array[WithParam] = actualParams

  /**
   * Get the tunnel parameters passed to the called template
   * @return the tunnel parameters
   */
  def getTunnelParams(): Array[WithParam] = tunnelParams

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    select = doPromotion(select, offer)
    WithParam.promoteParams(this, actualParams, offer)
    WithParam.promoteParams(this, tunnelParams, offer)
  }
}

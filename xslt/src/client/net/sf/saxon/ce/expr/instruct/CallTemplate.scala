// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.SequenceType
import java.util.ArrayList
import java.util.Arrays
import java.util.Iterator
import CallTemplate._
//remove if not needed
import scala.collection.JavaConversions._

object CallTemplate {

  /**
   * A CallTemplatePackage is an object that encapsulates the name of a template to be called,
   * the parameters to be supplied, and the execution context. This object can be returned as a tail
   * call, so that the actual call is made from a lower point on the stack, allowing a tail-recursive
   * template to execute in a finite stack size
   */
  class CallTemplatePackage(var target: Template, 
      var params: ParameterSet, 
      var tunnelParams: ParameterSet, 
      var evaluationContext: XPathContext) extends TailCall {

    def setEvaluationContext(evaluationContext: XPathContext) {
      this.evaluationContext = evaluationContext
    }

    /**
     * Process the template call encapsulated by this package.
     * @return another TailCall. This will never be the original call, but it may be the next
     * recursive call. For example, if A calls B which calls C which calls D, then B may return
     * a TailCall to A representing the call from B to C; when this is processed, the result may be
     * a TailCall representing the call from C to D.
     * @throws XPathException if a dynamic error occurs
     */
    def processLeavingTail(): TailCall = {
      val c2 = evaluationContext.newContext()
      c2.setParameters(target.getNumberOfSlots, params, tunnelParams)
      target.expand(c2)
    }
  }
}

/**
 * Instruction representing an xsl:call-template element in the stylesheet.
 */
class CallTemplate(var template: Template, var useTailRecursion: Boolean) extends Instruction {

  private var actualParams: Array[WithParam] = null

  private var tunnelParams: Array[WithParam] = null

  def setUseTailRecursion(useIt: Boolean) {
    useTailRecursion = useIt
  }

  /**
   * Set the actual parameters on the call
   * @param actualParams the parameters that are not tunnel parameters
   * @param tunnelParams the tunnel parameters
   */
  def setActualParameters(actualParams: Array[WithParam], tunnelParams: Array[WithParam]) {
    this.actualParams = actualParams
    this.tunnelParams = tunnelParams
    for (actualParam <- actualParams) {
      adoptChildExpression(actualParam)
    }
    for (tunnelParam <- tunnelParams) {
      adoptChildExpression(tunnelParam)
    }
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @exception XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    WithParam.simplify(actualParams, visitor)
    WithParam.simplify(tunnelParams, visitor)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    WithParam.typeCheck(actualParams, visitor, contextItemType)
    WithParam.typeCheck(tunnelParams, visitor, contextItemType)
    if (template.getBody != null) {
      val backwards = visitor.getStaticContext.isInBackwardsCompatibleMode
      for (p <- 0 until actualParams.length) {
        val wp = actualParams(p)
        val id = wp.getParameterId
        val lp = template.getLocalParam(id)
        if (lp != null) {
          val req = lp.getRequiredType
          val role = new RoleLocator(RoleLocator.PARAM, wp.getVariableQName.getDisplayName, p)
          val select = TypeChecker.staticTypeCheck(wp.getSelectExpression, req, backwards, role)
          wp.setSelectExpression(select)
          wp.setTypeChecked(true)
        }
      }
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    WithParam.optimize(visitor, actualParams, contextItemType)
    WithParam.optimize(visitor, tunnelParams, contextItemType)
    this
  }

  /**
   * Get the cardinality of the sequence returned by evaluating this instruction
   *
   * @return the static cardinality
   */
  def computeCardinality(): Int = {
    if (template == null) {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    } else {
      template.getRequiredType.getCardinality
    }
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   *
   * @return the static item type of the instruction
   */
  def getItemType(): ItemType = {
    if (template == null) {
      AnyItemType.getInstance
    } else {
      template.getRequiredType.getPrimaryType
    }
  }

  def getIntrinsicDependencies(): Int = {
    StaticProperty.DEPENDS_ON_XSLT_CONTEXT | StaticProperty.DEPENDS_ON_FOCUS
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation currently returns true unconditionally.
   */
  def createsNewNodes(): Boolean = true

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    val list = new ArrayList[Expression](10)
    WithParam.getXPathExpressions(actualParams, list)
    WithParam.getXPathExpressions(tunnelParams, list)
    list.iterator()
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  protected def promoteInst(offer: PromotionOffer) {
    WithParam.promoteParams(this, actualParams, offer)
    WithParam.promoteParams(this, tunnelParams, offer)
  }

  /**
   * Process this instruction, without leaving any tail calls.
   * @param context the dynamic context for this transformation
   * @throws XPathException if a dynamic error occurs
   */
  def process(context: XPathContext) {
    val t = getTargetTemplate
    val c2 = context.newContext()
    c2.setParameters(t.getNumberOfSlots, assembleParams(context, actualParams), assembleTunnelParams(context, 
      tunnelParams))
    var tc = t.expand(c2)
    while (tc != null) {
      tc = tc.processLeavingTail()
    }
  }

  /**
   * Process this instruction. If the called template contains a tail call (which may be
   * an xsl:call-template of xsl:apply-templates instruction) then the tail call will not
   * actually be evaluated, but will be returned in a TailCall object for the caller to execute.
   * @param context the dynamic context for this transformation
   * @return an object containing information about the tail call to be executed by the
   * caller. Returns null if there is no tail call.
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    if (!useTailRecursion) {
      process(context)
      return null
    }
    val target = getTargetTemplate
    var params = assembleParams(context, actualParams)
    val tunnels = assembleTunnelParams(context, tunnelParams)
    if (params == null) {
      params = ParameterSet.EMPTY_PARAMETER_SET
    }
    Arrays.fill(context.getStackFrame, null)
    new CallTemplatePackage(target, params, tunnels, context)
  }

  /**
   * Get the template, in the case where it is specified dynamically.
   * @return                  The template to be called
   * @throws XPathException if a dynamic error occurs: specifically, if the
   * template name is computed at run-time (Saxon extension) and the name is invalid
   * or does not reference a known template
   */
  def getTargetTemplate(): Template = template

  def getObjectName(): StructuredQName = {
    (if (template == null) null else template.getTemplateName)
  }
}

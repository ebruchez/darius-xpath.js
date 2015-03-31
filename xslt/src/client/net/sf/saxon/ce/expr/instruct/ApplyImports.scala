// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.Mode
import client.net.sf.saxon.ce.trans.Rule
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import java.util.ArrayList
import java.util.Iterator
import ApplyImports._
//remove if not needed
import scala.collection.JavaConversions._

object ApplyImports {

  val APPLY_IMPORTS = 1

  val NEXT_MATCH = 2
}

/**
 * An xsl:apply-imports or xsl:next-match instruction
 */
class ApplyImports(var operation: Int) extends Instruction {

  var actualParams: Array[WithParam] = null

  var tunnelParams: Array[WithParam] = null

  /**
   * Set the actual parameters on the call
   */
  def setActualParameters(actualParams: Array[WithParam], tunnelParams: Array[WithParam]): Unit = {
    this.actualParams = actualParams
    this.tunnelParams = tunnelParams
  }

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
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @exception client.net.sf.saxon.ce.trans.XPathException if an error is discovered during expression
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
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    WithParam.optimize(visitor, actualParams, contextItemType)
    WithParam.optimize(visitor, tunnelParams, contextItemType)
    this
  }

  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true (which is almost invariably the case, so it's not worth
   * doing any further analysis to find out more precisely).
   */
  def createsNewNodes(): Boolean = true

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    WithParam.promoteParams(this, actualParams, offer)
    WithParam.promoteParams(this, tunnelParams, offer)
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    val list = new ArrayList(10)
    WithParam.getXPathExpressions(actualParams, list)
    WithParam.getXPathExpressions(tunnelParams, list)
    list.iterator()
  }

  def processLeavingTail(context: XPathContext): TailCall = {
    var rule: Rule = null
    val controller = context.getController
    val params = assembleParams(context, actualParams)
    val tunnels = assembleTunnelParams(context, tunnelParams)
    val currentTemplateRule = context.getCurrentTemplateRule
    if (currentTemplateRule == null) {
      dynamicError("There is no current template rule", "XTDE0560")
      return null
    }
    var mode = context.getCurrentMode
    if (mode == null) {
      mode = controller.getRuleManager.getUnnamedMode
    }
    if (context.getCurrentIterator == null) {
      throw new XPathException("There is no context item", "XTDE0565")
    }
    val currentItem = context.getContextItem
    if (!(currentItem.isInstanceOf[NodeInfo])) {
      dynamicError("The context item is not a node", "XTDE0565")
    }
    if (operation == APPLY_IMPORTS) {
      val min = currentTemplateRule.getMinImportPrecedence
      val max = currentTemplateRule.getPrecedence - 1
      rule = controller.getRuleManager.getTemplateRule(currentItem.asInstanceOf[NodeInfo], mode, min, 
        max, context)
    } else {
      rule = controller.getRuleManager.getNextMatchHandler(currentItem.asInstanceOf[NodeInfo], mode, 
        currentTemplateRule, context)
    }
    if (rule == null) {
      ApplyTemplates.defaultAction(currentItem.asInstanceOf[NodeInfo], params, tunnels, context, getSourceLocator)
    } else {
      val nh = rule.getAction
      val c2 = context.newContext()
      c2.setParameters(nh.getNumberOfSlots, params, tunnels)
      c2.setCurrentTemplateRule(rule)
      nh.apply(c2)
    }
    null
  }
}

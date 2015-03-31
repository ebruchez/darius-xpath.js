// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.`type`.{ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.{LogConfiguration, LogController}

/**
 * Handler for xsl:for-each elements in a stylesheet.
 */
class ForEach extends Instruction with ContextMappingFunction {

  protected var select: Expression = _

  protected var action: Expression = _

  protected var containsTailCall: Boolean = _

  /**
   * Create an xsl:for-each instruction
   * @param select the select expression
   * @param action the body of the xsl:for-each loop
   */
  def this(select: Expression, action: Expression, containsTailCall: Boolean) {
    this()
    this.select = select
    this.action = action
    this.containsTailCall = containsTailCall
    adoptChildExpression(select)
    adoptChildExpression(action)
  }

  /**
   * Get the select expression
   * @return the select expression. Note this will have been wrapped in a sort expression
   * if sorting was requested.
   */
  def getSelectExpression(): Expression = select

  /**
   * Get the action expression (the content of the for-each)
   * @return the body of the for-each loop
   */
  def getActionExpression(): Expression = action

  /**
   * Determine the data type of the items returned by this expression
   * @return the data type
   */
  override def getItemType(): ItemType = action.getItemType

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true if the "action" creates new nodes.
   * (Nodes created by the condition can't contribute to the result).
   */
  override def createsNewNodes(): Boolean = {
    val props = action.getSpecialProperties
    ((props & StaticProperty.NON_CREATIVE) == 0)
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @throws XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor the expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    action = visitor.simplify(action)
    this
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    select = visitor.typeCheck(select, contextItemType)
    adoptChildExpression(select)
    action = visitor.typeCheck(action, select.getItemType)
    adoptChildExpression(action)
    if (Literal.isEmptySequence(select)) {
      return select
    }
    if (Literal.isEmptySequence(action)) {
      return action
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    select = visitor.optimize(select, contextItemType)
    adoptChildExpression(select)
    action = action.optimize(visitor, select.getItemType)
    adoptChildExpression(action)
    if (Literal.isEmptySequence(select)) {
      return select
    }
    if (Literal.isEmptySequence(action)) {
      return action
    }
    val offer = new PromotionOffer()
    offer.action = PromotionOffer.FOCUS_INDEPENDENT
    offer.promoteDocumentDependent = (select.getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0
    offer.promoteXSLTFunctions = false
    offer.containingExpression = this
    offer.bindingList = new Array[Binding](0)
    action = doPromotion(action, offer)
    if (offer.containingExpression.isInstanceOf[LetExpression]) {
      offer.containingExpression = visitor.optimize(offer.containingExpression, contextItemType)
    }
    val e2 = offer.containingExpression
    if (e2 != this) {
      return e2
    }
    this
  }

  /**
   * Compute the dependencies of an expression, as the union of the
   * dependencies of its subexpressions. (This is overridden for path expressions
   * and filter expressions, where the dependencies of a subexpression are not all
   * propogated). This method should be called only once, to compute the dependencies;
   * after that, getDependencies should be used.
   *
   * @return the depencies, as a bit-mask
   */
  override def computeDependencies(): Int = {
    var dependencies = 0
    dependencies |= select.getDependencies
    dependencies |= (action.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS)
    dependencies
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  override protected def promoteInst(offer: PromotionOffer): Unit = {
    select = doPromotion(select, offer)
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select, action)

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  override def hasLoopingSubexpression(child: Expression): Boolean = child == action

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD | Expression.PROCESS_METHOD

  def processLeavingTail(context: XPathContext): TailCall = {
    val iter = select.iterate(context)
    val c2 = context.newContext()
    val focus = c2.setCurrentIterator(iter)
    c2.setCurrentTemplateRule(null)
    if (containsTailCall) {
      if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
        val listener = LogController.getTraceListener
        val item = focus.next()
        if (item == null) {
          return null
        }
        listener.startCurrentItem(item)
        val tc = action.asInstanceOf[TailCallReturner].processLeavingTail(c2)
        listener.endCurrentItem(item)
        return tc
      } else {
        val item = focus.next()
        if (item == null) {
          return null
        }
      }
      return action.asInstanceOf[TailCallReturner].processLeavingTail(c2)
    } else {
      if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
        val listener = LogController.getTraceListener
        while (true) {
          val item = focus.next()
          if (item == null) {
            //break
          }
          listener.startCurrentItem(item)
          action.process(c2)
          listener.endCurrentItem(item)
        }
      } else {
        while (true) {
          val item = focus.next()
          if (item == null) {
            //break
          }
          action.process(c2)
        }
      }
    }
    null
  }

  /**
   * Return an Iterator to iterate over the values of the sequence.
   *
   * @throws XPathException if any dynamic error occurs evaluating the
   *     expression
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *     of the expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val master = select.iterate(context)
    val c2 = context.newContext()
    c2.setCurrentTemplateRule(null)
    c2.setCurrentIterator(master)
    new ContextMappingIterator(this, c2)
  }

  /**
   * Map one item to a sequence.
   * @param context The processing context. This is supplied only for mapping constructs that
   * set the context node, position, and size. Otherwise it is null.
   * @return either (a) a SequenceIterator over the sequence of items that the supplied input
   * item maps to, or (b) an Item if it maps to a single item, or (c) null if it maps to an empty
   * sequence.
   */
  def map(context: XPathContext): SequenceIterator = action.iterate(context)
}

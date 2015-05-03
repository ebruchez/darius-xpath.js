// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr.instruct.{Instruction, TailCall}
import org.orbeon.darius.xpath.om.{Item, NamespaceResolver, SequenceIterator, StructuredQName}
import org.orbeon.darius.xpath.orbeon.{HashMap, Iterator}
import org.orbeon.darius.xpath.trace.InstructionInfo
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.{LogConfiguration, LogController}

import scala.beans.BeanProperty

/**
 * A wrapper expression used to trace expressions in XPath and XQuery.
 */
class TraceExpression(var child: Expression) extends Instruction with InstructionInfo {

  @BeanProperty
  var objectName: StructuredQName = _

  @BeanProperty
  var constructType: StructuredQName = _

  @BeanProperty
  var namespaceResolver: NamespaceResolver = null

  private val properties: HashMap[String, AnyRef] = new HashMap[String, AnyRef](10)

  adoptChildExpression(child)

  setProperty("expression", child)

  /**
   * Set a named property of the instruction/expression
   * @param name the name of the property
   * @param value the value of the property
   */
  def setProperty(name: String, value: AnyRef): Unit = {
    properties.put(name, value)
  }

  /**
   * Get a named property of the instruction/expression
   * @param name the name of the property
   * @return the value of the property
   */
  def getProperty(name: String): AnyRef = properties.get(name)

  /**
   * Get an iterator over all the properties available. The values returned by the iterator
   * will be of type String, and each string can be supplied as input to the getProperty()
   * method to retrieve the value of the property.
   */
  def getProperties: Iterator[String] = properties.keysIterator()

  /**
   * Get the InstructionInfo details about the construct. This is to satisfy the InstructionInfoProvider
   * interface.
   * @return the instruction details
   */
  def getInstructionInfo: InstructionInfo = this

  override def simplify(visitor: ExpressionVisitor): Expression = {
    child = visitor.simplify(child)
    if (child.isInstanceOf[TraceExpression]) {
      return child
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    child = visitor.typeCheck(child, contextItemType)
    adoptChildExpression(child)
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    child = visitor.optimize(child, contextItemType)
    adoptChildExpression(child)
    this
  }

  override def getImplementationMethod: Int = child.getImplementationMethod

  /**
   * Offer promotion for this subexpression. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *              expressions that don't depend on the context to an outer level in
   *              the containing expression
   * @param parent the parent of the subexpression
   * @return if the offer is not accepted, return this expression unchanged.
   *         Otherwise return the result of rewriting the expression to promote
   *         this subexpression
   * @throws XPathException
   *          if any error is detected
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val newChild = child.promote(offer, parent)
    if (newChild != child) {
      child = newChild
      adoptChildExpression(child)
      return this
    }
    this
  }

  /**
   * Execute this instruction, with the possibility of returning tail calls if there are any.
   * This outputs the trace information via the registered TraceListener,
   * and invokes the instruction being traced.
   * @param context the dynamic execution context
   * @return either null, or a tail call that the caller must invoke on return
   * @throws XPathException
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    val controller = context.getController
    assert(controller != null)
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      val listener = LogController.getTraceListener
      listener.enter(getInstructionInfo, context)
      child.process(context)
      listener.leave(getInstructionInfo)
    } else {
      child.process(context)
    }
    null
  }

  override def getItemType: ItemType = child.getItemType

  /**
   * Determine the static cardinality of the expression. This establishes how many items
   * there will be in the result of the expression, at compile time (i.e., without
   * actually evaluating the result.
   *
   * @return one of the values Cardinality.ONE_OR_MORE,
   *         Cardinality.ZERO_OR_MORE, Cardinality.EXACTLY_ONE,
   *         Cardinality.ZERO_OR_ONE, Cardinality.EMPTY. This default
   *         implementation returns ZERO_OR_MORE (which effectively gives no
   *         information).
   */
  override def getCardinality: Int = child.getCardinality

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as [[StaticProperty.DEPENDS_ON_CONTEXT_ITEM]] and
   * [[StaticProperty.DEPENDS_ON_CURRENT_ITEM]]. The default implementation combines the intrinsic
   * dependencies of this expression with the dependencies of the subexpressions,
   * computed recursively. This is overridden for expressions such as FilterExpression
   * where a subexpression's dependencies are not necessarily inherited by the parent
   * expression.
   *
   * @return a set of bit-significant flags identifying the dependencies of
   *     the expression
   */
  override def getDependencies: Int = child.getDependencies

  /**
   * Determine whether this instruction creates new nodes.
   *
   *
   */
  override def createsNewNodes(): Boolean = {
    (child.getSpecialProperties & StaticProperty.NON_CREATIVE) == 
      0
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  override def computeDependencies(): Int = child.computeDependencies()

  /**
   * Evaluate an expression as a single item. This always returns either a single Item or
   * null (denoting the empty sequence). No conversion is done. This method should not be
   * used unless the static type of the expression is a subtype of "item" or "item?": that is,
   * it should not be called if the expression may return a sequence. There is no guarantee that
   * this condition will be detected.
   *
   * @param context The context in which the expression is to be evaluated
   * @throws XPathException if any dynamic error occurs evaluating the
   *     expression
   * @return the node or atomic value that results from evaluating the
   *     expression; or null to indicate that the result is an empty
   *     sequence
   */
  override def evaluateItem(context: XPathContext): Item = {
    var result: Item = null
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      LogController.getTraceListener.enter(getInstructionInfo, context)
      result = child.evaluateItem(context)
      LogController.getTraceListener.leave(getInstructionInfo)
    } else {
      result = child.evaluateItem(context)
    }
    result
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    var result: SequenceIterator = null
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      LogController.getTraceListener.enter(getInstructionInfo, context)
      result = child.iterate(context)
      LogController.getTraceListener.leave(getInstructionInfo)
    } else {
      result = child.iterate(context)
    }
    result
  }

  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(child)

//ORBEON XSLT
//  /**
//   * Evaluate an updating expression, adding the results to a Pending Update List.
//   * The default implementation of this method, which is used for non-updating expressions,
//   * throws an UnsupportedOperationException
//   *
//   * @param context the XPath dynamic evaluation context
//   * @param pul     the pending update list to which the results should be written
//   */
//  def evaluatePendingUpdates(context: XPathContext, pul: PendingUpdateList) {
//    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
//      LogController.getTraceListener.enter(getInstructionInfo, context)
//      child.evaluatePendingUpdates(context, pul)
//      LogController.getTraceListener.leave(getInstructionInfo)
//    } else {
//      child.evaluatePendingUpdates(context, pul)
//    }
//  }

  def getLineNumber: Int = 0
}

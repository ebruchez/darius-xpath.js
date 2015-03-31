// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr.ForExpression._
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator, StructuredQName}
import client.net.sf.saxon.ce.value.{Cardinality, IntegerValue, SequenceType}

object ForExpression {

  /**
   * The MappingAction represents the action to be taken for each item in the
   * source sequence. It acts as the MappingFunction for the mapping iterator, and
   * also as the Binding of the position variable (at $n) in XQuery, if used.
   */
  protected class MappingAction(var context: XPathContext, 
      var slotNumber: Int, 
      var pslot: Int, 
      var action: Expression) extends MappingFunction with ItemMappingFunction with StatefulMappingFunction {

    private var position: Int = 1

    def map(item: Item): SequenceIterator = {
      context.setLocalVariable(slotNumber, item)
      if (pslot >= 0) {
        val value = position
        position += 1
        context.setLocalVariable(pslot, new IntegerValue(value))
      }
      action.iterate(context)
    }

    def mapItem(item: Item): Item = {
      context.setLocalVariable(slotNumber, item)
      if (pslot >= 0) {
        val value = position
        position += 1
        context.setLocalVariable(pslot, new IntegerValue(value))
      }
      action.evaluateItem(context)
    }

    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = {
      val c2 = context.newContext()
      val vars = context.getStackFrame
      val newvars = new Array[Sequence](vars.length)
      System.arraycopy(vars, 0, newvars, 0, vars.length)
      c2.setStackFrame(newvars.length, newvars)
      new MappingAction(c2, slotNumber, pslot, action)
    }
  }
}

/**
 * A ForExpression maps an expression over a sequence.
 * This version works with range variables, it doesn't change the context information
 */
class ForExpression extends Assignation {

  var actionCardinality: Int = StaticProperty.ALLOWS_MANY

  /**
   * Set the slot number for the range variable
   * @param nr the slot number allocated to the range variable on the local stack frame.
   * This implicitly allocates the next slot number to the position variable if there is one.
   */
  override def setSlotNumber(nr: Int) {
    super.setSlotNumber(nr)
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    sequence = visitor.typeCheck(sequence, contextItemType)
    if (Literal.isEmptySequence(sequence)) {
      return sequence
    }
    if (requiredType != null) {
      val th = TypeHierarchy.getInstance
      val decl = requiredType
      val sequenceType = SequenceType.makeSequenceType(decl.getPrimaryType, StaticProperty.ALLOWS_ZERO_OR_MORE)
      val role = new RoleLocator(RoleLocator.VARIABLE, variableName, 0)
      val actualItemType = sequence.getItemType
      refineTypeInformation(actualItemType, getRangeVariableCardinality, null, sequence.getSpecialProperties, 
        visitor, this)
    }
    action = visitor.typeCheck(action, contextItemType)
    if (Literal.isEmptySequence(action)) {
      return action
    }
    actionCardinality = action.getCardinality
    this
  }

  /**
   * Get the cardinality of the range variable
   * @return the cardinality of the range variable (StaticProperty.EXACTLY_ONE). Can be overridden
   * in a subclass
   */
  protected def getRangeVariableCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Optimize the expression
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val seq2 = visitor.optimize(sequence, contextItemType)
    if (seq2 != sequence) {
      sequence = seq2
      adoptChildExpression(sequence)
      visitor.resetStaticProperties()
      return optimize(visitor, contextItemType)
    }
    if (Literal.isEmptySequence(sequence)) {
      return sequence
    }
    val act2 = visitor.optimize(action, contextItemType)
    if (act2 != action) {
      action = act2
      adoptChildExpression(action)
      visitor.resetStaticProperties()
      return optimize(visitor, contextItemType)
    }
    if (Literal.isEmptySequence(action)) {
      return action
    }
    val e2 = extractLoopInvariants(visitor, contextItemType)
    if (e2 != null && e2 != this) {
      return visitor.optimize(e2, contextItemType)
    }
    if (action.isInstanceOf[VariableReference] && 
      action.asInstanceOf[VariableReference].getBinding == this) {
      return sequence
    }
    if (sequence.getCardinality == StaticProperty.EXACTLY_ONE) {
      val let = new LetExpression()
      let.setVariableQName(variableName)
      let.setRequiredType(SequenceType.makeSequenceType(sequence.getItemType, StaticProperty.EXACTLY_ONE))
      let.setSequence(sequence)
      let.setAction(action)
      let.setSlotNumber(slotNumber)
      ExpressionTool.rebindVariableReferences(action, this, let)
      return let.optimize(visitor, contextItemType)
    }
    this
  }

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  override def hasLoopingSubexpression(child: Expression): Boolean = child == action

  /**
   * Extract subexpressions in the action part that don't depend on the range variable
   * @param visitor the expression visitor
   * @param contextItemType the item type of the context item
   * @return the optimized expression if it has changed, or null if no optimization was possible
   */
  private def extractLoopInvariants(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val offer = new PromotionOffer()
    offer.containingExpression = this
    offer.action = PromotionOffer.RANGE_INDEPENDENT
    offer.bindingList = Array(this)
    action = doPromotion(action, offer)
    if (offer.containingExpression.isInstanceOf[LetExpression]) {
      offer.containingExpression = visitor.optimize(offer.containingExpression, contextItemType)
    }
    offer.containingExpression
  }

  /**
   * Mark tail function calls: only possible if the for expression iterates zero or one times.
   * (This arises in XSLT/XPath, which does not have a LET expression, so FOR gets used instead)
   */
  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int = {
    if (!Cardinality.allowsMany(sequence.getCardinality)) {
      ExpressionTool.markTailFunctionCalls(action, qName, arity)
    } else {
      0
    }
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD | Expression.PROCESS_METHOD

  /**
   * Iterate over the sequence of values
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = sequence.iterate(context)
    val pslot = -1
    val map = new MappingAction(context, getLocalSlotNumber, pslot, action)
    actionCardinality match {
      case StaticProperty.EXACTLY_ONE => new ItemMappingIterator(base, map, true)
      case StaticProperty.ALLOWS_ZERO_OR_ONE => new ItemMappingIterator(base, map, false)
      case _ => new MappingIterator(base, map)
    }
  }

  /**
   * Process this expression as an instruction, writing results to the current
   * outputter
   */
  override def process(context: XPathContext) {
    val iter = sequence.iterate(context)
    var position = 1
    val slot = getLocalSlotNumber
    val pslot = -1
    while (true) {
      val item = iter.next()
      if (item == null)
        return
      context.setLocalVariable(slot, item)
      if (pslot >= 0) {
        val value = position
        position += 1
        context.setLocalVariable(pslot, new IntegerValue(value))
      }
      action.process(context)
    }
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   * @return one of the values Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   * or Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = action.getItemType

  /**
   * Determine the static cardinality of the expression
   */
  def computeCardinality(): Int = {
    val c1 = sequence.getCardinality
    val c2 = action.getCardinality
    Cardinality.multiply(c1, c2)
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   * @return a representation of the expression as a string
   */
  override def toString(): String = {
    "for $" + getVariableName + " in " + 
      (if (sequence == null) "(...)" else sequence.toString) + 
      " return " + 
      (if (action == null) "(...)" else action.toString)
  }
}

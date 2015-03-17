// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, Type, TypeHierarchy}
import client.net.sf.saxon.ce.expr.instruct.ForEach
import client.net.sf.saxon.ce.expr.sort.{DocumentOrderIterator, GlobalOrderComparer}
import client.net.sf.saxon.ce.om.{Item, NodeInfo, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.{EmptyIterator, OneItemGoneIterator}
import client.net.sf.saxon.ce.tree.util.SourceLocator
import client.net.sf.saxon.ce.value.{AtomicValue, Cardinality, EmptySequence, SequenceType}

/**
 * A slash expression is any expression using the binary slash operator "/". The parser initially generates a slash
 * expression for all occurrences of the binary "/" operator. Subsequently, as a result of type inferencing, the
 * majority of slash expressions will be rewritten as instances of PathExpression (returning nodes) or
 * ForEach instructions (when they return atomic values). However, in the rare case where it is not possible to determine
 * statically whether the rh operand returns nodes or atomic values, instances of this class may need to be interpreted
 * directly at run time.
 */
class SlashExpression(var start: Expression, var step: Expression) extends Expression with ContextMappingFunction {

  adoptChildExpression(start)

  adoptChildExpression(step)

  protected def setStartExpression(start2: Expression) {
    if (start != start2) {
      start = start2
      adoptChildExpression(start)
    }
  }

  protected def setStepExpression(step2: Expression) {
    if (step != step2) {
      step = step2
      adoptChildExpression(step)
    }
  }

  /**
   * Get the start expression (the left-hand operand)
   * @return the first operand
   */
  def getControllingExpression(): Expression = start

  /**
   * Get the step expression (the right-hand operand)
   * @return the second operand
   */
  def getControlledExpression(): Expression = step

  /**
   * Determine whether this expression is capable (as far as static analysis is concerned)
   * of returning a mixture of nodes and atomic values. If so, this needs to be prevented
   * at run time
   * @return true if the static type allows both nodes and atomic values
   */
  def isHybrid(): Boolean = true

  /**
   * Simplify an expression
   * @return the simplified expression
   * @param visitor the expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    setStartExpression(visitor.simplify(start))
    setStepExpression(visitor.simplify(step))
    if (Literal.isEmptySequence(start)) {
      return start
    }
    if (Literal.isEmptySequence(step)) {
      return step
    }
    if (start.isInstanceOf[RootExpression] && step.isInstanceOf[ParentNodeExpression]) {
      return Literal.makeEmptySequence()
    }
    this
  }

  /**
   * Determine the data type of the items returned by this exprssion
   * @return the type of the step
   */
  def getItemType(): ItemType = step.getItemType

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    var start2 = visitor.typeCheck(start, contextItemType)
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, "/", 0)
    role0.setErrorCode("XPTY0019")
    setStartExpression(TypeChecker.staticTypeCheck(start2, SequenceType.NODE_SEQUENCE, false, role0))
    setStepExpression(visitor.typeCheck(step, start.getItemType))
    val stepType = step.getItemType
    if (th.isSubType(stepType, Type.NODE_TYPE)) {
      if ((step.getSpecialProperties & StaticProperty.NON_CREATIVE) != 
        0) {
        val config = visitor.getConfiguration
        start2 = ExpressionTool.unsorted(config, start, false)
        val step2 = ExpressionTool.unsorted(config, step, false)
        val path = new PathExpression(start2, step2)
        ExpressionTool.copyLocationInfo(this, path)
        var sortedPath = path.addDocumentSorter()
        ExpressionTool.copyLocationInfo(this, sortedPath)
        sortedPath = sortedPath.simplify(visitor)
        sortedPath.typeCheck(visitor, contextItemType)
      } else {
        val path = new PathExpression(start, step)
        ExpressionTool.copyLocationInfo(this, path)
        var sortedPath = path.addDocumentSorter()
        ExpressionTool.copyLocationInfo(this, sortedPath)
        sortedPath = sortedPath.simplify(visitor)
        sortedPath.typeCheck(visitor, contextItemType)
      }
    } else if (stepType.isInstanceOf[AtomicType]) {
      val ame = new ForEach(start, step, false)
      ExpressionTool.copyLocationInfo(this, ame)
      visitor.typeCheck(visitor.simplify(ame), contextItemType)
    } else {
      this
    }
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    setStartExpression(visitor.optimize(start, contextItemType))
    setStepExpression(step.optimize(visitor, start.getItemType))
    if (Literal.isEmptySequence(start) || Literal.isEmptySequence(step)) {
      return new Literal(EmptySequence.getInstance)
    }
    promoteFocusIndependentSubexpressions(visitor, contextItemType)
  }

  /**
   * If any subexpressions within the step are not dependent on the focus,
   * and if they are not "creative" expressions (expressions that can create new nodes), then
   * promote them: this causes them to be evaluated once, outside the path expression
   * @param visitor the expression visitor
   * @param contextItemType the type of the context item for evaluating the start expression
   * @return the rewritten expression, or the original expression if no rewrite was possible
   */
  protected def promoteFocusIndependentSubexpressions(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val config = visitor.getConfiguration
    val offer = new PromotionOffer()
    offer.action = PromotionOffer.FOCUS_INDEPENDENT
    offer.promoteDocumentDependent = (start.getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0
    offer.containingExpression = this
    setStepExpression(doPromotion(step, offer))
    visitor.resetStaticProperties()
    if (offer.containingExpression != this) {
      offer.containingExpression = visitor.optimize(visitor.typeCheck(offer.containingExpression, contextItemType), 
        contextItemType)
      return offer.containingExpression
    }
    this
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      setStartExpression(doPromotion(start, offer))
      if (offer.action == PromotionOffer.REPLACE_CURRENT) {
        setStepExpression(doPromotion(step, offer))
      }
      this
    }
  }

  /**
   * Get the immediate subexpressions of this expression
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(start, step)

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  override def hasLoopingSubexpression(child: Expression): Boolean = child == step

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as XPathContext.VARIABLES and
   * XPathContext.CURRENT_NODE
   */
  override def computeDependencies(): Int = {
    start.getDependencies | 
      (step.getDependencies & 
      (StaticProperty.DEPENDS_ON_XSLT_CONTEXT | StaticProperty.DEPENDS_ON_LOCAL_VARIABLES | 
      StaticProperty.DEPENDS_ON_USER_FUNCTIONS))
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    var p = super.computeSpecialProperties()
    if ((start.getSpecialProperties & step.getSpecialProperties & 
      StaticProperty.NON_CREATIVE) != 
      0) {
      p |= StaticProperty.NON_CREATIVE
    }
    p
  }

  /**
   * Determine the static cardinality of the expression
   */
  def computeCardinality(): Int = {
    val c1 = start.getCardinality
    val c2 = step.getCardinality
    Cardinality.multiply(c1, c2)
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[SlashExpression])) {
      return false
    }
    val p = other.asInstanceOf[SlashExpression]
    (start == p.start && step == p.step)
  }

  /**
   * get HashCode for comparing two expressions
   */
  override def hashCode(): Int = {
    "SlashExpression".hashCode + start.hashCode + step.hashCode
  }

  /**
   * Iterate the path-expression in a given context
   * @param context the evaluation context
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    var result = start.iterate(context)
    val context2 = context.newMinorContext()
    context2.setCurrentIterator(result)
    result = new ContextMappingIterator(this, context2)
    val loc = getSourceLocator
    val first = result.next()
    if (first == null) {
      EmptyIterator.getInstance
    } else if (first.isInstanceOf[AtomicValue]) {
      val atomicValueChecker = new ItemMappingFunction() {

        def mapItem(item: Item): Item = {
          if (item.isInstanceOf[AtomicValue]) {
            return item
          } else {
            throw reportMixedItems(loc)
          }
        }
      }
      new ItemMappingIterator(new OneItemGoneIterator(first, result), atomicValueChecker, true)
    } else {
      val nodeChecker = new ItemMappingFunction() {

        def mapItem(item: Item): Item = {
          if (item.isInstanceOf[NodeInfo]) {
            return item
          } else {
            throw reportMixedItems(loc)
          }
        }
      }
      new DocumentOrderIterator(new ItemMappingIterator(new OneItemGoneIterator(first, result), nodeChecker, 
        true), GlobalOrderComparer.getInstance)
    }
  }

  private def reportMixedItems(loc: SourceLocator): XPathException = {
    new XPathException("Cannot mix nodes and atomic values in the result of a path expression", "XPTY0018", 
      loc)
  }

  /**
   * Mapping function, from a node returned by the start iteration, to a sequence
   * returned by the child.
   */
  def map(context: XPathContext): SequenceIterator = step.iterate(context)

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   * @return a representation of the expression as a string
   */
  override def toString(): String = start.toString + "/" + step.toString

  /**
   * Get the first step in this expression. A path expression A/B/C is represented as (A/B)/C, but
   * the first step is A
   * @return the first step in the expression, after expanding any nested path expressions
   */
  def getFirstStep(): Expression = {
    if (start.isInstanceOf[SlashExpression]) {
      start.asInstanceOf[SlashExpression].getFirstStep
    } else {
      start
    }
  }

  /**
   * Get all steps after the first.
   * This is complicated by the fact that A/B/C is represented as ((A/B)/C; we are required
   * to return B/C
   * @return a path expression containing all steps in this path expression other than the first,
   * after expanding any nested path expressions
   */
  def getRemainingSteps(): Expression = {
    if (start.isInstanceOf[SlashExpression]) {
      val rem = new SlashExpression(start.asInstanceOf[PathExpression].getRemainingSteps, step)
      ExpressionTool.copyLocationInfo(start, rem)
      rem
    } else {
      step
    }
  }

  /**
   * Get the last step of the path expression
   * @return the last step in the expression, after expanding any nested path expressions
   */
  def getLastStep(): Expression = {
    if (step.isInstanceOf[SlashExpression]) {
      step.asInstanceOf[SlashExpression].getLastStep
    } else {
      step
    }
  }

  /**
   * Get a path expression consisting of all steps except the last
   * @return a path expression containing all steps in this path expression other than the last,
   * after expanding any nested path expressions
   */
  def getLeadingSteps(): Expression = {
    if (step.isInstanceOf[SlashExpression]) {
      val rem = new SlashExpression(start, step.asInstanceOf[SlashExpression].getLeadingSteps)
      ExpressionTool.copyLocationInfo(start, rem)
      rem
    } else {
      start
    }
  }
}

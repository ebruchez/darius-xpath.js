// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import java.{util ⇒ ju}

import org.orbeon.darius.xpath.`type`.{ItemType, TypeHierarchy}
import org.orbeon.darius.xpath.expr.PathExpression._
import org.orbeon.darius.xpath.expr.sort.DocumentSorter
import org.orbeon.darius.xpath.functions.SystemFunction
import org.orbeon.darius.xpath.om.{Axis, SequenceIterator}
import org.orbeon.darius.xpath.pattern.{AnyNodeTest, NodeKindTest}
import org.orbeon.darius.xpath.value.{Cardinality, EmptySequence, SequenceType}

object PathExpression {

  /**
   * Determine whether an expression is an
   * axis step with optional filter predicates.
   * @param _exp the expression to be examined
   * @return true if the supplied expression is an AxisExpression, or an AxisExpression wrapped by one
   *         or more filter expressions
   */
  private def isFilteredAxisPath(_exp: Expression): Boolean = {
    var exp = _exp
    if (exp.isInstanceOf[AxisExpression]) {
      true
    } else {
      while (exp.isInstanceOf[FilterExpression]) {
        exp = exp.asInstanceOf[FilterExpression].getControllingExpression
      }
      exp.isInstanceOf[AxisExpression]
    }
  }
}

/**
 * An expression that establishes a set of nodes by following relationships between nodes
 * in the document. Specifically, it consists of a start expression which defines a set of
 * nodes, and a step which defines a relationship to be followed from those nodes to create
 * a new set of nodes.
 * <p/>
 * <p>This class inherits from SlashExpression; it is used in the common case where the SlashExpression
 * is known to return nodes rather than atomic values.</p>
 * <p/>
 * <p>This class is not responsible for sorting the results into document order or removing duplicates.
 * That is done by a DocumentSorter expression which is wrapped around the path expression. However, this
 * class does contain the logic for deciding statically whether the DocumentSorter is needed or not.</p>
 */
class PathExpression(start: Expression, step: Expression) extends SlashExpression(start, step) with ContextMappingFunction {

  private var state: Int = 0

  step match {
    case stepPath: PathExpression ⇒
      if (isFilteredAxisPath(stepPath.getControllingExpression) &&
        isFilteredAxisPath(stepPath.getControlledExpression)) {
        setStartExpression(new PathExpression(start, stepPath.start))
        setStepExpression(stepPath.step)
      }
    case _ ⇒
  }

  override def isHybrid: Boolean = false

  /**
   * Add a document sorting node to the expression tree, if needed
   */
  def addDocumentSorter(): Expression = {
    val props = getSpecialProperties
    if ((props & StaticProperty.ORDERED_NODESET) != 0) {
      this
    } else if ((props & StaticProperty.REVERSE_DOCUMENT_ORDER) != 0) {
      SystemFunction.makeSystemFunction("reverse", Array(this))
    } else {
      new DocumentSorter(this)
    }
  }

  /**
   * Simplify an expression
   * @param visitor the expression visitor
   * @return the simplified expression
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    if (state > 0) {
      return this
    }
    state = 1
    val e2 = super.simplify(visitor)
    if (e2 != this) {
      return e2
    }
    if (start.isInstanceOf[ContextItemExpression]) {
      return step
    }
    if (step.isInstanceOf[ContextItemExpression]) {
      return start
    }
    this
  }

  private def simplifyDescendantPath(env: StaticContext): PathExpression = {
    var st = start
    start match {
      case stax: AxisExpression ⇒
        if (stax.getAxis != Axis.DESCENDANT_OR_SELF) {
          return null
        }
        val cie = new ContextItemExpression()
        ExpressionTool.copyLocationInfo(this, cie)
        st = new PathExpression(cie, stax)
        ExpressionTool.copyLocationInfo(this, st)
      case _ ⇒
    }
    if (!st.isInstanceOf[PathExpression]) {
      return null
    }
    val startPath = st.asInstanceOf[PathExpression]
    if (!startPath.step.isInstanceOf[AxisExpression]) {
      return null
    }
    val mid = startPath.step.asInstanceOf[AxisExpression]
    if (mid.getAxis != Axis.DESCENDANT_OR_SELF) {
      return null
    }
    val test = mid.getNodeTest
    if (!(test == null || test.isInstanceOf[AnyNodeTest])) {
      return null
    }
    var underlyingStep = step
    while (underlyingStep.isInstanceOf[FilterExpression]) {
      if (underlyingStep.asInstanceOf[FilterExpression].isPositional(TypeHierarchy.getInstance)) {
        return null
      }
      underlyingStep = underlyingStep.asInstanceOf[FilterExpression].getControllingExpression
    }
    if (!underlyingStep.isInstanceOf[AxisExpression]) {
      return null
    }
    val underlyingAxis = underlyingStep.asInstanceOf[AxisExpression]
    if (underlyingAxis.getAxis == Axis.CHILD) {
      var newStep: Expression = new AxisExpression(Axis.DESCENDANT, underlyingStep.asInstanceOf[AxisExpression].getNodeTest)
      ExpressionTool.copyLocationInfo(this, newStep)
      underlyingStep = step
      val filters = new ju.LinkedList[Expression]()
      while (underlyingStep.isInstanceOf[FilterExpression]) {
        filters.addLast(underlyingStep.asInstanceOf[FilterExpression].getFilter)
        underlyingStep = underlyingStep.asInstanceOf[FilterExpression].getControllingExpression
      }
      while (! filters.isEmpty) {
        newStep = new FilterExpression(newStep, filters.pop())
        ExpressionTool.copyLocationInfo(step, newStep)
      }
      val newPath = new PathExpression(startPath.start, newStep)
      ExpressionTool.copyLocationInfo(this, newPath)
      return newPath
    }
    if (underlyingAxis.getAxis == Axis.ATTRIBUTE) {
      val newStep = new AxisExpression(Axis.DESCENDANT_OR_SELF, NodeKindTest.ELEMENT)
      ExpressionTool.copyLocationInfo(this, newStep)
      val newPath = new PathExpression(new PathExpression(startPath.start, newStep), step)
      ExpressionTool.copyLocationInfo(this, newPath)
      return newPath
    }
    null
  }

  /**
   * Perform type analysis
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    if (state >= 2) {
      setStartExpression(visitor.typeCheck(start, contextItemType))
      setStepExpression(visitor.typeCheck(step, start.getItemType))
      return this
    }
    state = 2
    setStartExpression(visitor.typeCheck(start, contextItemType))
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, "/", 0)
    role0.setErrorCode("XPTY0019")
    setStartExpression(TypeChecker.staticTypeCheck(start, SequenceType.NODE_SEQUENCE, backwardsCompatible = false, role0))
    setStepExpression(visitor.typeCheck(step, start.getItemType))
    if (start.isInstanceOf[ContextItemExpression]) {
      return step
    }
    if ((step.getSpecialProperties & StaticProperty.NON_CREATIVE) !=
      0) {
      val config = visitor.getConfiguration
      setStartExpression(ExpressionTool.unsorted(config, start, retainAllNodes = false))
      setStepExpression(ExpressionTool.unsorted(config, step, retainAllNodes = false))
      val p = simplifyDescendantPath(visitor.getStaticContext)
      if (p != null) {
        ExpressionTool.copyLocationInfo(this, p)
        return visitor.typeCheck(visitor.simplify(p), contextItemType)
      } else {
        adoptChildExpression(start)
        adoptChildExpression(step)
      }
    }
    this
  }

  /**
   * Optimize the expression and perform type analysis
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    if (state >= 3) {
      setStartExpression(visitor.optimize(start, contextItemType))
      setStepExpression(step.optimize(visitor, start.getItemType))
      return this
    }
    state = 3
    val lastStep = getLastStep
    lastStep match {
      case filterExpression: FilterExpression if !filterExpression.isPositional(th) ⇒
        val leading = getLeadingSteps
        val p2 = new PathExpression(leading, filterExpression.getControllingExpression)
        val f2 = new FilterExpression(p2, filterExpression.getFilter)
        return f2.optimize(visitor, contextItemType)
      case _ ⇒
    }
    setStartExpression(visitor.optimize(start, contextItemType))
    setStepExpression(step.optimize(visitor, start.getItemType))
    if (Literal.isEmptySequence(start) || Literal.isEmptySequence(step)) {
      return new Literal(EmptySequence.getInstance)
    }
    promoteFocusIndependentSubexpressions(visitor, contextItemType)
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val p = this
    val exp = offer.accept(parent, p)
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
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    var startProperties = start.getSpecialProperties
    var stepProperties = step.getSpecialProperties
    var p = 0
    if (!Cardinality.allowsMany(start.getCardinality)) {
      startProperties |= StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET |
        StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    if (!Cardinality.allowsMany(step.getCardinality)) {
      stepProperties |= StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET |
        StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    if ((startProperties & stepProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
      0) {
      p |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    if (((startProperties & StaticProperty.SINGLE_DOCUMENT_NODESET) !=
      0) &&
      ((stepProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
      0)) {
      p |= StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    if ((startProperties & stepProperties & StaticProperty.PEER_NODESET) !=
      0) {
      p |= StaticProperty.PEER_NODESET
    }
    if ((startProperties & stepProperties & StaticProperty.SUBTREE_NODESET) !=
      0) {
      p |= StaticProperty.SUBTREE_NODESET
    }
    if (testNaturallySorted(startProperties, stepProperties)) {
      p |= StaticProperty.ORDERED_NODESET
    }
    if (testNaturallyReverseSorted()) {
      p |= StaticProperty.REVERSE_DOCUMENT_ORDER
    }
    if ((startProperties & stepProperties & StaticProperty.NON_CREATIVE) !=
      0) {
      p |= StaticProperty.NON_CREATIVE
    }
    p
  }

  /**
   * Determine if we can guarantee that the nodes are delivered in document order.
   * This is true if the start nodes are sorted peer nodes
   * and the step is based on an Axis within the subtree rooted at each node.
   * It is also true if the start is a singleton node and the axis is sorted.
   * @param startProperties the properties of the left-hand expression
   * @param stepProperties  the properties of the right-hand expression
   * @return true if the natural nested-loop evaluation strategy for the expression
   *         is known to deliver results with no duplicates and in document order, that is,
   *         if no additional sort is required
   */
  private def testNaturallySorted(startProperties: Int, stepProperties: Int): Boolean = {
    if ((stepProperties & StaticProperty.ORDERED_NODESET) == 0) {
      return false
    }
    if (Cardinality.allowsMany(start.getCardinality)) {
      if ((startProperties & StaticProperty.ORDERED_NODESET) == 0) {
        return false
      }
    } else {
      return true
    }
    if ((stepProperties & StaticProperty.ATTRIBUTE_NS_NODESET) !=
      0) {
      return true
    }
    ((startProperties & StaticProperty.PEER_NODESET) != 0) &&
      ((stepProperties & StaticProperty.SUBTREE_NODESET) != 0)
  }

  /**
   * Determine if the path expression naturally returns nodes in reverse document order
   * @return true if the natural nested-loop evaluation strategy returns nodes in reverse
   *         document order
   */
  private def testNaturallyReverseSorted(): Boolean = {
    if (!Cardinality.allowsMany(start.getCardinality) && step.isInstanceOf[AxisExpression]) {
      return !Axis.isForwards(step.asInstanceOf[AxisExpression].getAxis)
    }
    !Cardinality.allowsMany(step.getCardinality) && start.isInstanceOf[AxisExpression] &&
      !Axis.isForwards(start.asInstanceOf[AxisExpression].getAxis)
  }

  /**
   * Get all steps after the first.
   * This is complicated by the fact that A/B/C is represented as ((A/B)/C; we are required
   * to return B/C
   * @return a path expression containing all steps in this path expression other than the first,
   *         after expanding any nested path expressions
   */
  override def getRemainingSteps: Expression = {
    start match {
      case pathExpression: PathExpression ⇒
        val rem = new PathExpression(pathExpression.getRemainingSteps, step)
        ExpressionTool.copyLocationInfo(start, rem)
        rem
      case _ ⇒
        step
    }
  }

  /**
   * Test whether a path expression is an absolute path - that is, a path whose first step selects a
   * document node
   * @param th the type hierarchy cache
   * @return true if the first step in this path expression selects a document node
   */
  def isAbsolute(th: TypeHierarchy): Boolean = {
    val first = getFirstStep
    if (th.isSubType(first.getItemType, NodeKindTest.DOCUMENT)) {
      return true
    }
    false
  }

  /**
   * Iterate the path-expression in a given context
   * @param context the evaluation context
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val master = start.iterate(context)
    val context2 = context.newMinorContext()
    context2.setCurrentIterator(master)
    new ContextMappingIterator(this, context2)
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString: String = {
    "(" + start.toString + "/" + step.toString + ")"
  }
}

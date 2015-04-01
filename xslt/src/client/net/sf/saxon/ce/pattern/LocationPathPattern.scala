// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import java.util.{ArrayList, Arrays, Iterator}

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, Type, TypeHierarchy}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.{Last, Position}
import client.net.sf.saxon.ce.om.{Axis, NodeInfo}
import client.net.sf.saxon.ce.pattern.LocationPathPattern._
import client.net.sf.saxon.ce.trans.XPathException

import scala.beans.BeanProperty

object LocationPathPattern {
  private val EMPTY_FILTER_ARRAY: Array[Expression] = new Array[Expression](0)
}

/**
 * A LocationPathPattern represents a path, for example of the form A/B/C... The components are represented
 * as a linked list, each component pointing to its predecessor
 */
class LocationPathPattern extends Pattern {

  @BeanProperty
  var upperPattern: Pattern = null

  @BeanProperty
  var upwardsAxis: Byte = Axis.PARENT

  var nodeTest: NodeTest = AnyNodeTest.getInstance

  protected var filters: Array[Expression] = EMPTY_FILTER_ARRAY

  protected var equivalentExpr: Expression = null

  protected var firstElementPattern: Boolean = false

  protected var lastElementPattern: Boolean = false

  protected var specialFilter: Boolean = false

  private var refinedNodeTest: NodeTest = null

  /**
   * Set the NodeTest
   * @param test the NodeTest
   */
  def setNodeTest(test: NodeTest): Unit = {
    if (test == null) {
      throw new NullPointerException("test")
    }
    this.nodeTest = test
  }

  /**
   * Set the superior pattern (matching a parent or ancestor node
   * @param axis the axis (parent or ancestor) connecting to the upper pattern
   * @param upper the pattern that a parent or ancestor must match
   */
  def setUpperPattern(axis: Byte, upper: Pattern): Unit = {
    this.upwardsAxis = axis
    this.upperPattern = upper
  }

  /**
   * Add a filter to the pattern (while under construction)
   *
   * @param filter The predicate (a boolean expression or numeric expression) to be added
   */
  def addFilter(filter: Expression): Unit = {
    val len = filters.length
    val f2 = Array.ofDim[Expression](len + 1)
    System.arraycopy(filters, 0, f2, 0, len)
    filters = f2
    filters(len) = filter
    filter.setContainer(this)
  }

  override def setSystemId(systemId: String): Unit = {
    super.setSystemId(systemId)
    if (upperPattern != null) {
      upperPattern.setSystemId(systemId)
    }
  }

  /**
   * Set the executable containing this pattern
   *
   * @param executable the executable
   */
  def setExecutable(executable: Executable): Unit = {
    super.setExecutable(executable)
    if (upperPattern != null) {
      upperPattern.setExecutable(executable)
    }
  }

  /**
   * Get the filters assocated with the last step in the pattern
   * @return an array of expression holding the filter predicates in order
   */
  def getFilters(): Array[Expression] = filters

  /**
   * Simplify the pattern: perform any context-independent optimisations
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Pattern = {
    if (upperPattern == null && filters.length == 0 && !firstElementPattern && 
      !lastElementPattern) {
      val ntp = new NodeTestPattern(nodeTest)
      ntp.setSystemId(getSystemId)
      return ntp
    }
    if (upperPattern != null) {
      upperPattern = upperPattern.simplify(visitor)
    }
    var i = filters.length - 1
    while (i >= 0) {
      filters(i) = visitor.simplify(filters(i))
      i -= 1
    }
    this
  }

  /**
   * Type-check the pattern, performing any type-dependent optimizations.
   * @param visitor an expression visitor
   * @param contextItemType the type of the context item at the point where the pattern appears
   * @return the optimised Pattern
   */
  override def analyze(visitor: ExpressionVisitor, contextItemType: ItemType): Pattern = {
    val th = TypeHierarchy.getInstance
    if (upperPattern != null) {
      upperPattern = upperPattern.analyze(visitor, contextItemType)
      if (upwardsAxis == Axis.PARENT) {
        var step: AxisExpression = null
        step = if (nodeTest.getRequiredNodeKind == Type.ATTRIBUTE) new AxisExpression(Axis.ATTRIBUTE, 
          nodeTest) else new AxisExpression(Axis.CHILD, nodeTest)
        step.setSourceLocator(this)
        step.setContainer(this)
        val exp = visitor.typeCheck(step, upperPattern.getNodeTest)
        refinedNodeTest = exp.getItemType.asInstanceOf[NodeTest]
      }
    }
    var removeEntries = 0
    var i = filters.length - 1
    while (i >= 0) {
      var filter = visitor.typeCheck(filters(i), getNodeTest)
      filter = visitor.optimize(filter, getNodeTest)
      filters(i) = filter
      if (Literal.isConstantBoolean(filter, value = true)) {
        removeEntries += 1
      } else if (Literal.isConstantBoolean(filter, value = false)) {
        return new NodeTestPattern(EmptySequenceTest.getInstance)
      }
      i -= 1
    }
    if (removeEntries > 0) {
      if (removeEntries == filters.length) {
        filters = EMPTY_FILTER_ARRAY
      } else {
        val f2 = Array.ofDim[Expression](filters.length - removeEntries)
        var j = 0
        for (i ← 0 until filters.length if !Literal.isConstantBoolean(filters(i), value = true)) {
          f2(j) = filters(i)
          j += 1
        }
        filters = f2
      }
    }
    if (nodeTest.getRequiredNodeKind == Type.ELEMENT && filters.length == 1) {
      if (Literal.isConstantOne(filters(0))) {
        firstElementPattern = true
        specialFilter = true
        filters = EMPTY_FILTER_ARRAY
      } else if (filters(0).isInstanceOf[ComparisonExpression]) {
        val comp = filters(0).asInstanceOf[ComparisonExpression]
        if (comp.getSingletonOperator == Token.FEQ && 
          (comp.getOperands()(0).isInstanceOf[Position] && Literal.isConstantOne(comp.getOperands()(1))) || 
          (comp.getOperands()(1).isInstanceOf[Position] && Literal.isConstantOne(comp.getOperands()(0)))) {
          firstElementPattern = true
          specialFilter = true
          filters = EMPTY_FILTER_ARRAY
        }
      }
    }
    if (nodeTest.getRequiredNodeKind == Type.ELEMENT && filters.length == 1 && 
      filters(0).isInstanceOf[Last]) {
      lastElementPattern = true
      specialFilter = true
      filters = EMPTY_FILTER_ARRAY
    }
    if (isPositional(th)) {
      equivalentExpr = makeEquivalentExpression()
      equivalentExpr = visitor.typeCheck(equivalentExpr, contextItemType)
      specialFilter = true
    }
    this
  }

  /**
   * Get the dependencies of the pattern. The only possible dependency for a pattern is
   * on local variables. This is analyzed in those patterns where local variables may appear.
   */
  override def getDependencies(): Int = {
    var dependencies = 0
    if (upperPattern != null) {
      dependencies |= upperPattern.getDependencies
    }
    for (i ← 0 until filters.length) {
      dependencies |= filters(i).getDependencies
    }
    dependencies &= StaticProperty.DEPENDS_ON_LOCAL_VARIABLES
    dependencies
  }

  /**
   * Iterate over the subexpressions within this pattern
   */
  override def iterateSubExpressions(): Iterator[Expression] = {
    val list = new ArrayList[Expression]()
    if (getVariableBindingExpression != null) {
      list.add(getVariableBindingExpression)
    }
    list.addAll(Arrays.asList(filters:_*))
    if (upperPattern != null) {
      var upper = upperPattern.iterateSubExpressions()
      while (upper.hasNext) {
        list.add(upper.next())
      }
    }
    list.iterator()
  }

  /**
   * Allocate slots to any variables used within the pattern
   * @param _nextFree the next slot that is free to be allocated @return the next slot that is free to be allocated
   */
  override def allocateSlots(_nextFree: Int): Int = {
    var nextFree = _nextFree
    if (getVariableBindingExpression != null) {
      nextFree = ExpressionTool.allocateSlots(getVariableBindingExpression, nextFree)
    }
    for (i ← 0 until filters.length) {
      nextFree = ExpressionTool.allocateSlots(filters(i), nextFree)
    }
    if (upperPattern != null) {
      nextFree = upperPattern.allocateSlots(nextFree)
    }
    nextFree
  }

  /**
   * Offer promotion for subexpressions within this pattern. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   * <p/>
   * <p>Unlike the corresponding method on [[client.net.sf.saxon.ce.expr.Expression]], this method does not return anything:
   * it can make internal changes to the pattern, but cannot return a different pattern. Only certain
   * kinds of promotion are applicable within a pattern: specifically, promotions affecting local
   * variable references within the pattern.
   *
   * @param offer details of the offer, for example the offer to move
   *              expressions that don't depend on the context to an outer level in
   *              the containing expression
   * @param parent
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any error is detected
   */
  override def promote(offer: PromotionOffer, parent: Expression): Unit = {
    if (upperPattern != null) {
      upperPattern.promote(offer, parent)
    }
    val savedBindingList = offer.bindingList
    if (getVariableBindingExpression.isInstanceOf[Assignation]) {
      offer.bindingList = getVariableBindingExpression.asInstanceOf[Assignation]
        .extendBindingList(offer.bindingList)
    }
    for (i ← 0 until filters.length) {
      filters(i) = filters(i).promote(offer, parent)
    }
    offer.bindingList = savedBindingList
  }

  /**
   * For a positional pattern, make an equivalent path expression to evaluate the filters.
   * This expression takes the node being tested as the context node, and returns a set of nodes
   * which will include the context node if and only if it matches the pattern. The expression only
   * tests nodes against the filters, not against any parent or ancestor patterns.
   * @return the equivalent path expression
   */
  private def makeEquivalentExpression(): Expression = {
    val axis = if (nodeTest.getRequiredNodeKind == Type.ATTRIBUTE) Axis.ATTRIBUTE else Axis.CHILD
    var step: Expression = new AxisExpression(axis, nodeTest)
    for (n ← 0 until filters.length) {
      step = new FilterExpression(step, filters(n))
    }
    val start = new ParentNodeExpression()
    start.setContainer(this)
    val path = new PathExpression(start, step)
    path.setContainer(this)
    path
  }

  /**
   * Determine whether the pattern matches a given node.
   *
   *
   * @param node the node to be tested
   * @return true if the pattern matches, else false
   */
  def matches(node: NodeInfo, context: XPathContext): Boolean = {
    bindCurrent(node, context)
    internalMatches(node, null, context)
  }

  /**
   * Test whether the pattern matches, but without changing the current() node
   */
  override protected def internalMatches(node: NodeInfo, anchor: NodeInfo, context: XPathContext): Boolean = {
    if (!nodeTest.matches(node)) {
      return false
    }
    if (upperPattern != null) {
      var anc = node
      if (upwardsAxis == Axis.PARENT || upwardsAxis == Axis.ANCESTOR) {
        anc = node.getParent
      }
      while (true) {
        if (anc == null) {
          return false
        }
        if (upperPattern.internalMatches(anc, anchor, context)) {
          //break
        }
        if (upwardsAxis == Axis.PARENT) {
          return false
        }
        anc = anc.getParent
      }
    }
    if (specialFilter) {
      if (firstElementPattern) {
        val iter = node.iterateAxis(Axis.PRECEDING_SIBLING, nodeTest)
        return iter.next() == null
      }
      if (lastElementPattern) {
        val iter = node.iterateAxis(Axis.FOLLOWING_SIBLING, nodeTest)
        return iter.next() == null
      }
      if (equivalentExpr != null) {
        val c2 = context.newMinorContext()
        c2.setSingletonFocus(node)
        try {
          val nsv = equivalentExpr.iterate(c2)
          while (true) {
            val n = nsv.next().asInstanceOf[NodeInfo]
            if (n == null) {
              return false
            }
            if (n.isSameNodeInfo(node)) {
              return true
            }
          }
        } catch {
          case e: XPathException ⇒ return false
        }
      }
    }
    if (filters.length != 0) {
      val c2 = context.newMinorContext()
      c2.setSingletonFocus(node)
      for (filter ← filters) {
        try {
          if (!filter.effectiveBooleanValue(c2)) {
            return false
          }
        } catch {
          case e: XPathException ⇒ return false
        }
      }
    }
    true
  }

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * For patterns that match nodes of several types, return Node.NODE
   *
   * @return the type of node matched by this pattern. e.g. Node.ELEMENT or Node.TEXT
   */
  override def getNodeKind(): Int = nodeTest.getRequiredNodeKind

  /**
   * Get a NodeTest that all the nodes matching this pattern must satisfy
   */
  def getNodeTest(): NodeTest = {
    if (refinedNodeTest != null) {
      refinedNodeTest
    } else {
      nodeTest
    }
  }

  /**
   * Determine if the pattern uses positional filters
   * @param th the type hierarchy cache
   * @return true if there is a numeric filter in the pattern, or one that uses the position()
   *         or last() functions
   */
  def isPositional(th: TypeHierarchy): Boolean = {
    for (i ← 0 until filters.length) {
      val `type` = filters(i).getItemType
      if (`type` == AtomicType.DOUBLE || `type` == AtomicType.DECIMAL || 
        `type` == AtomicType.INTEGER || 
        `type` == AtomicType.FLOAT || 
        `type` == AtomicType.ANY_ATOMIC) {
        return true
      }
      if ((filters(i).getDependencies & 
        (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST)) != 
        0) {
        return true
      }
    }
    false
  }

  /**
   * If the pattern contains any calls on current(), this method is called to modify such calls
   * to become variable references to a variable declared in a specially-allocated local variable
   *
   * @param let   the expression that assigns the local variable. This returns a dummy result, and is executed
   *              just before evaluating the pattern, to get the value of the context item into the variable.
   * @param offer A PromotionOffer used to process the expressions and change the call on current() into
   *              a variable reference
   * @param topLevel
   * @throws XPathException
   */
  override def resolveCurrent(let: LetExpression, offer: PromotionOffer, topLevel: Boolean): Unit = {
    for (i ← 0 until filters.length) {
      filters(i) = filters(i).promote(offer, let)
    }
    if (upperPattern.isInstanceOf[LocationPathPattern]) {
      upperPattern.resolveCurrent(let, offer, false)
    }
    if (topLevel) {
      setVariableBindingExpression(let)
    }
  }

  /**
   * Determine whether this pattern is the same as another pattern
   * @param other the other object
   */
  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[LocationPathPattern]) {
      val lpp = other.asInstanceOf[LocationPathPattern]
      if (! filters.sameElements(lpp.filters)) {
        return false
      }
      if (nodeTest != lpp.nodeTest) {
        return false
      }
      if (upwardsAxis != lpp.upwardsAxis) {
        return false
      }
      if (upperPattern == null) {
        if (lpp.upperPattern != null) {
          return false
        }
      } else {
        if (upperPattern != lpp.upperPattern) {
          return false
        }
      }
    } else {
      return false
    }
    true
  }

  /**
   * hashcode supporting equals()
   */
  override def hashCode(): Int = {
    var h = 88267
    for (i ← 0 until filters.length) {
      h ^= filters(i).hashCode
    }
    h ^= nodeTest.hashCode
    if (upperPattern != null) {
      h ^= upperPattern.hashCode
    }
    h ^= (upwardsAxis << 22)
    h
  }
}

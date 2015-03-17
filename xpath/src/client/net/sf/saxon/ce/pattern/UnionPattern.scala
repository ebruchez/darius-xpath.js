package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import java.util._
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A pattern formed as the union (or) of two other patterns
 */
class UnionPattern(protected var p1: Pattern, protected var p2: Pattern) extends Pattern {

  private var nodeType: Int = Type.NODE

  if (p1.getNodeKind == p2.getNodeKind) {
    nodeType = p1.getNodeKind
  }

  /**
   * Set the executable containing this pattern
   *
   * @param executable the executable
   */
  def setExecutable(executable: Executable) {
    p1.setExecutable(executable)
    p2.setExecutable(executable)
    super.setExecutable(executable)
  }

  /**
   * Simplify the pattern: perform any context-independent optimisations
   *
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Pattern = {
    p1 = p1.simplify(visitor)
    p2 = p2.simplify(visitor)
    this
  }

  /**
   * Type-check the pattern.
   * This is only needed for patterns that contain variable references or function calls.
   *
   * @return the optimised Pattern
   */
  def analyze(visitor: ExpressionVisitor, contextItemType: ItemType): Pattern = {
    p1 = p1.analyze(visitor, contextItemType)
    p2 = p2.analyze(visitor, contextItemType)
    this
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
  def resolveCurrent(let: LetExpression, offer: PromotionOffer, topLevel: Boolean) {
    p1.resolveCurrent(let, offer, false)
    p2.resolveCurrent(let, offer, false)
    if (topLevel) {
      setVariableBindingExpression(let)
    }
  }

  /**
   * Offer promotion for subexpressions within this pattern. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   * <p/>
   * <p>Unlike the corresponding method on {@link client.net.sf.saxon.ce.expr.Expression}, this method does not return anything:
   * it can make internal changes to the pattern, but cannot return a different pattern. Only certain
   * kinds of promotion are applicable within a pattern: specifically, promotions affecting local
   * variable references within the pattern.
   * @param offer details of the offer, for example the offer to move
   *              expressions that don't depend on the context to an outer level in
   *              the containing expression
   * @param parent
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any error is detected
   */
  def promote(offer: PromotionOffer, parent: Expression) {
    p1.promote(offer, parent)
    p2.promote(offer, parent)
  }

  /**
   * Set the original text
   */
  def setOriginalText(pattern: String) {
    super.setOriginalText(pattern)
    p1.setOriginalText(pattern)
    p2.setOriginalText(pattern)
  }

  /**
   * Allocate slots to any variables used within the pattern
   *
   * @param nextFree the next slot that is free to be allocated @return the next slot that is free to be allocated
   */
  def allocateSlots(nextFree: Int): Int = {
    if (getVariableBindingExpression != null) {
      nextFree = ExpressionTool.allocateSlots(getVariableBindingExpression, nextFree)
    }
    nextFree = p1.allocateSlots(nextFree)
    nextFree = p2.allocateSlots(nextFree)
    nextFree
  }

  /**
   * Gather the component (non-union) patterns of this union pattern
   * @param set the set into which the components will be added
   */
  def gatherComponentPatterns(set: Set) {
    if (p1.isInstanceOf[UnionPattern]) {
      p1.asInstanceOf[UnionPattern].gatherComponentPatterns(set)
    } else {
      set.add(p1)
    }
    if (p2.isInstanceOf[UnionPattern]) {
      p2.asInstanceOf[UnionPattern].gatherComponentPatterns(set)
    } else {
      set.add(p2)
    }
  }

  /**
   * Determine if the supplied node matches the pattern
   *
   *
   * @param e the node to be compared
   * @return true if the node matches either of the operand patterns
   */
  def matches(e: NodeInfo, context: XPathContext): Boolean = {
    bindCurrent(e, context)
    p1.matches(e, context) || p2.matches(e, context)
  }

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * For patterns that match nodes of several types, return Node.NODE
   *
   * @return the type of node matched by this pattern. e.g. Node.ELEMENT or Node.TEXT
   */
  def getNodeKind(): Int = nodeType

  /**
   * Get a NodeTest that all the nodes matching this pattern must satisfy
   */
  def getNodeTest(): NodeTest = {
    if (nodeType == Type.NODE) {
      AnyNodeTest.getInstance
    } else {
      NodeKindTest.makeNodeKindTest(nodeType)
    }
  }

  /**
   * Get the dependencies of the pattern. The only possible dependency for a pattern is
   * on local variables. This is analyzed in those patterns where local variables may appear.
   *
   * @return the dependencies, as a bit-significant mask
   */
  def getDependencies(): Int = p1.getDependencies | p2.getDependencies

  /**
   * Iterate over the subexpressions within this pattern
   * @return an iterator over the subexpressions.
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    val list = new ArrayList[Expression]()
    if (getVariableBindingExpression != null) {
      list.add(getVariableBindingExpression)
    }
    var i1 = p1.iterateSubExpressions()
    while (i1.hasNext) {
      list.add(i1.next())
    }
    var i2 = p2.iterateSubExpressions()
    while (i2.hasNext) {
      list.add(i2.next())
    }
    list.iterator()
  }

  /**
   * Get the LHS of the union
   *
   * @return the first operand of the union
   */
  def getLHS(): Pattern = p1

  /**
   * Get the RHS of the union
   *
   * @return the second operand of the union
   */
  def getRHS(): Pattern = p2

  /**
   * Override method to set the system ID, so it's set on both halves
   */
  def setSystemId(systemId: String) {
    super.setSystemId(systemId)
    p1.setSystemId(systemId)
    p2.setSystemId(systemId)
  }

  /**
   * Determine whether this pattern is the same as another pattern
   * @param other the other object
   */
  override def equals(other: Any): Boolean = other match {
    case other: UnionPattern => {
      val s0 = new HashSet(10)
      gatherComponentPatterns(s0)
      val s1 = new HashSet(10)
      other.gatherComponentPatterns(s1)
      s0 == s1
    }
    case _ => false
  }

  /**
   * Hashcode supporting equals()
   */
  override def hashCode(): Int = 0x9bd723a6 ^ p1.hashCode ^ p2.hashCode
}

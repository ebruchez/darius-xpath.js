// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.SequenceType
import java.util.Arrays
import java.util.Iterator

import scala.collection.JavaConversions._

/**
 * A NodeSetPattern is a pattern based on an expression that is evaluated to return a set of nodes;
 * a node matches the pattern if it is a member of this node-set.
 *
 * <p>In XSLT 2.0 there are two forms of NodeSetPattern allowed, represented by calls on the id() and
 * key() functions. In XSLT 3.0, additional forms are allowed, for example a variable reference, and
 * a call to the doc() function. This class provides the general capability to use any expression
 * at the head of a pattern. This is used also to support streaming, where streaming XPath expressions
 * are mapped to patterns.</p>
 */
class NodeSetPattern(protected var expression: Expression) extends Pattern {

  protected var itemType: ItemType = _

  /**
   * Type-check the pattern.
   * Default implementation does nothing. This is only needed for patterns that contain
   * variable references or function calls.
   * @return the optimised Pattern
   */
  override def analyze(visitor: ExpressionVisitor, contextItemType: ItemType): Pattern = {
    expression = visitor.typeCheck(expression, contextItemType)
    val role = new RoleLocator(RoleLocator.VARIABLE, expression.toString, 0)
    expression = TypeChecker.staticTypeCheck(expression, SequenceType.NODE_SEQUENCE, false, role)
    itemType = expression.getItemType
    this
  }

  /**
   * Get the dependencies of the pattern. The only possible dependency for a pattern is
   * on local variables. This is analyzed in those patterns where local variables may appear.
   */
  override def getDependencies(): Int = expression.getDependencies

  /**
   * Iterate over the subexpressions within this pattern
   */
  override def iterateSubExpressions(): Iterator[Expression] = Arrays.asList(expression).iterator()

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
    expression = expression.promote(offer, parent)
  }

  /**
   * Allocate slots to any variables used within the pattern
   * @param nextFree    the next slot that is free to be allocated @return the next slot that is free to be allocated
   */
  override def allocateSlots(nextFree: Int): Int = {
    ExpressionTool.allocateSlots(expression, nextFree)
  }

  /**
   * Determine whether this Pattern matches the given Node
   *
   * @param e The NodeInfo representing the Element or other node to be tested against the Pattern
   * @return true if the node matches the Pattern, false otherwise
   */
  def matches(e: NodeInfo, context: XPathContext): Boolean = {
    try {
      val iter = expression.iterate(context)
      while (true) {
        val node = iter.next().asInstanceOf[NodeInfo]
        if (node == null) {
          return false
        }
        if (node.isSameNodeInfo(e)) {
          return true
        }
      }
      throw new IllegalStateException
    } catch {
      case err: XPathException ⇒ false
    }
  }

  /**
   * Determine the type of nodes to which this pattern applies.
   * @return Type.NODE (may be overridden in subclasses)
   */
  override def getNodeKind(): Int = {
    if (itemType.isInstanceOf[NodeTest]) {
      itemType.asInstanceOf[NodeTest].getRequiredNodeKind
    } else {
      Type.NODE
    }
  }

  /**
   * Get a NodeTest that all the nodes matching this pattern must satisfy
   */
  def getNodeTest(): NodeTest = {
    if (itemType.isInstanceOf[NodeTest]) {
      itemType.asInstanceOf[NodeTest]
    } else {
      AnyNodeTest.getInstance
    }
  }

  /**
   * Determine whether this pattern is the same as another pattern
   * @param other the other object
   */
  override def equals(other: Any): Boolean = {
    other.isInstanceOf[NodeSetPattern] &&
      other.asInstanceOf[NodeSetPattern].expression == expression
  }

  /**
   * Hashcode supporting equals()
   */
  override def hashCode(): Int = 0x73108728 ^ expression.hashCode
}

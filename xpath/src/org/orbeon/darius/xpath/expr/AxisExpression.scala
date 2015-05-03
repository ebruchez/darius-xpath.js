// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`.{AtomicType, ItemType, Type}
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.pattern.{AnyNodeTest, NameTest, NodeKindTest, NodeTest}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.AtomicValue

import scala.beans.BeanProperty

/**
 * An AxisExpression is always obtained by simplifying a PathExpression.
 * It represents a PathExpression that starts at the context node, and uses
 * a simple node-test with no filters. For example "*", "title", "./item",
 * "@*", or "ancestor::chapter*".
 * <p/>
 * <p>An AxisExpression delivers nodes in axis order (not in document order).
 * To get nodes in document order, in the case of a reverse axis, the expression
 * should be wrapped in a call on reverse().</p>
 */
class AxisExpression(@BeanProperty var axis: Byte, var test: NodeTest) extends Expression {

  private var itemType: ItemType = null

  @BeanProperty
  var contextItemType: ItemType = null

  var computedCardinality: Int = -1

  private var doneWarnings: Boolean = false

  /**
   * Simplify an expression
   *
   * @param visitor an expression visitor
   * @return the simplified expression
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    if (axis == Axis.PARENT && (test == null || test.isInstanceOf[AnyNodeTest])) {
      val p = new ParentNodeExpression()
      ExpressionTool.copyLocationInfo(this, p)
      return p
    }
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (contextItemType == null) {
      typeError(visitor, "Axis step " + toString + 
        " cannot be used here: the context item is undefined", "XPDY0002")
    }
    if (contextItemType.isInstanceOf[AtomicType]) {
      typeError(visitor, "Axis step " + toString + 
        " cannot be used here: the context item is an atomic value", "XPTY0020")
    }
    if (this.contextItemType == contextItemType && doneWarnings) {
      return this
    }
    this.contextItemType = contextItemType
    doneWarnings = true
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor         an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[org.orbeon.darius.xpath.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    if (! other.isInstanceOf[AxisExpression]) {
      return false
    }
    if (axis != other.asInstanceOf[AxisExpression].axis) {
      return false
    }
    if (test == null) {
      return other.asInstanceOf[AxisExpression].test == null
    }
    test.toString == other.asInstanceOf[AxisExpression].test.toString
  }

  /**
   * get HashCode for comparing two expressions
   */
  override def hashCode(): Int = {
    var h = 9375162 + axis << 20
    if (test != null) {
      h ^= test.hashCode
    }
    h
  }

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as XPathContext.VARIABLES and
   * XPathContext.CURRENT_NODE
   */
  override def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  private def copy(): Expression = {
    val a2 = new AxisExpression(axis, test)
    a2.itemType = itemType
    a2.contextItemType = contextItemType
    a2.computedCardinality = computedCardinality
    a2.doneWarnings = false
    a2
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    StaticProperty.CONTEXT_DOCUMENT_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.NON_CREATIVE | 
      (if (Axis.isForwards(axis)) StaticProperty.ORDERED_NODESET else StaticProperty.REVERSE_DOCUMENT_ORDER) | 
      (if (Axis.isPeerAxis(axis)) StaticProperty.PEER_NODESET else 0) | 
      (if (Axis.isSubtreeAxis(axis)) StaticProperty.SUBTREE_NODESET else 0) | 
      (if (axis == Axis.ATTRIBUTE || axis == Axis.NAMESPACE) StaticProperty.ATTRIBUTE_NS_NODESET else 0)
  }

  /**
   * Determine the data type of the items returned by this expression
   *
   * @return Type.NODE or a subtype, based on the NodeTest in the axis step, plus
   *         information about the content type if this is known from schema analysis
   */
  def getItemType: ItemType = {
    if (itemType != null) {
      return itemType
    }
    val p = Axis.principalNodeType(axis)
    p match {
      case Type.ATTRIBUTE | Type.NAMESPACE ⇒ NodeKindTest.makeNodeKindTest(p)
      case _ ⇒ if (test == null) {
        AnyNodeTest.getInstance
      } else {
        test
      }
    }
  }

  /**
   * Determine the cardinality of the result of this expression
   */
  def computeCardinality(): Int = {
    if (computedCardinality != -1) {
      return computedCardinality
    }
    if (axis == Axis.ATTRIBUTE && test.isInstanceOf[NameTest]) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else if (axis == Axis.SELF) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    }
  }

  /**
   * Get the NodeTest. Returns null if the AxisExpression can return any node.
   *
   * @return the node test, or null if all nodes are returned
   */
  def getNodeTest: NodeTest = test

  /**
   * Evaluate the path-expression in a given context to return a NodeSet
   *
   * @param context the evaluation context
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val item = context.getContextItem
    item match {
      case info: NodeInfo ⇒
        if (test == null) {
          info.iterateAxis(axis, AnyNodeTest.getInstance)
        } else {
          info.iterateAxis(axis, test)
        }
      case _ ⇒
        val cName = toString
        val isAtomic = item.isInstanceOf[AtomicValue]
        val appendText = " is " + (if (isAtomic) "not a node" else "undefined")
        val code = if (isAtomic) "XPTY0020" else "XPDY0002"
        val err = new XPathException("The context item for axis step " + cName + appendText)
        err.setErrorCode(code)
        err.setLocator(getSourceLocator)
        err.setIsTypeError(true)
        throw err
    }
  }

  /**
   * Represent the expression as a string for diagnostics
   */
  override def toString: String = {
    Axis.axisName(axis) + "::" + (if (test == null) "node()" else test.toString)
  }
}

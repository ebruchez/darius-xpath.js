// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{ItemType, Type}
import client.net.sf.saxon.ce.expr.instruct.Block
import client.net.sf.saxon.ce.expr.sort.{DocumentOrderIterator, GlobalOrderComparer}
import client.net.sf.saxon.ce.om.{Axis, SequenceIterator}
import client.net.sf.saxon.ce.value.SequenceType

/**
 * An expression representing a nodeset that is a union, difference, or
 * intersection of two other NodeSets
 */
class VennExpression(p1: Expression, op: Int, p2: Expression) extends BinaryExpression(p1, op, p2) {

  /**
   * Determine the data type of the items returned by this expression
   * @return the data type
   */
  def getItemType: ItemType = {
    val t1 = operand0.getItemType
    val t2 = operand1.getItemType
    Type.getCommonSuperType(t1, t2)
  }

  /**
   * Determine the static cardinality of the expression
   */
  override def computeCardinality(): Int = {
    val c1 = operand0.getCardinality
    val c2 = operand1.getCardinality
    operator match {
      case Token.UNION ⇒
        if (Literal.isEmptySequence(operand0)) return c2
        if (Literal.isEmptySequence(operand1)) return c1
        return c1 | c2 | StaticProperty.ALLOWS_ONE | StaticProperty.ALLOWS_MANY

      case Token.INTERSECT ⇒
        if (Literal.isEmptySequence(operand0)) return StaticProperty.EMPTY
        if (Literal.isEmptySequence(operand1)) return StaticProperty.EMPTY
        return (c1 & c2) | StaticProperty.ALLOWS_ZERO | StaticProperty.ALLOWS_ONE

      case Token.EXCEPT ⇒
        if (Literal.isEmptySequence(operand0)) return StaticProperty.EMPTY
        if (Literal.isEmptySequence(operand1)) return c1
        return c1 | StaticProperty.ALLOWS_ZERO | StaticProperty.ALLOWS_ONE

    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    val prop0 = operand0.getSpecialProperties
    val prop1 = operand1.getSpecialProperties
    var props = StaticProperty.ORDERED_NODESET
    if (testContextDocumentNodeSet(prop0, prop1)) {
      props |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    if (testSubTree(prop0, prop1)) {
      props |= StaticProperty.SUBTREE_NODESET
    }
    if (!testCreative(prop0, prop1)) {
      props |= StaticProperty.NON_CREATIVE
    }
    props
  }

  /**
   * Determine whether all the nodes in the node-set are guaranteed to
   * come from the same document as the context node. Used for optimization.
   * @param prop0 contains the Context Document Nodeset property of the first operand
   * @param prop1 contains the Context Document Nodeset property of the second operand
   * @return true if all the nodes come from the context document
   */
  private def testContextDocumentNodeSet(prop0: Int, prop1: Int): Boolean = operator match {
    case Token.UNION ⇒ (prop0 & prop1 & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
      0
    case Token.INTERSECT ⇒ ((prop0 | prop1) & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
      0
    case Token.EXCEPT ⇒ (prop0 & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0
  }

  /**
   * Determine whether all the nodes in the node-set are guaranteed to
   * come from a subtree rooted at the context node. Used for optimization.
   * @param prop0 contains the SubTree property of the first operand
   * @param prop1 contains the SubTree property of the second operand
   * @return true if all the nodes come from the tree rooted at the context node
   */
  private def testSubTree(prop0: Int, prop1: Int): Boolean = operator match {
    case Token.UNION ⇒ (prop0 & prop1 & StaticProperty.SUBTREE_NODESET) != 0
    case Token.INTERSECT ⇒ ((prop0 | prop1) & StaticProperty.SUBTREE_NODESET) != 0
    case Token.EXCEPT ⇒ (prop0 & StaticProperty.SUBTREE_NODESET) != 0
  }

  /**
   * Determine whether the expression can create new nodes
   * @param prop0 contains the noncreative property of the first operand
   * @param prop1 contains the noncreative property of the second operand
   * @return true if the expression can create new nodes
   */
  private def testCreative(prop0: Int, prop1: Int): Boolean = {
    !(((prop0 & StaticProperty.NON_CREATIVE) != 0) && ((prop1 & StaticProperty.NON_CREATIVE) != 0))
  }

  /**
   * Simplify the expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand0 = visitor.simplify(operand0)
    operand1 = visitor.simplify(operand1)
    operator match {
      case Token.UNION ⇒
        if (Literal.isEmptySequence(operand0) && 
          (operand1.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
          0) return operand1
        if (Literal.isEmptySequence(operand1) && 
          (operand0.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
          0) return operand0

      case Token.INTERSECT ⇒
        if (Literal.isEmptySequence(operand0)) return operand0
        if (Literal.isEmptySequence(operand1)) return operand1

      case Token.EXCEPT ⇒
        if (Literal.isEmptySequence(operand0)) return operand0
        if (Literal.isEmptySequence(operand1) && 
          (operand0.getSpecialProperties & StaticProperty.ORDERED_NODESET) != 
          0) return operand0

    }
    operand0 match {
      case path1: PathExpression if operand1.isInstanceOf[PathExpression] && operator == Token.UNION ⇒
        val path2 = operand1.asInstanceOf[PathExpression]
        if (path1.getFirstStep == path2.getFirstStep) {
          val venn = new VennExpression(path1.getRemainingSteps, operator, path2.getRemainingSteps)
          ExpressionTool.copyLocationInfo(this, venn)
          val path = new PathExpression(path1.getFirstStep, venn)
          ExpressionTool.copyLocationInfo(this, path)
          return visitor.simplify(path)
        }
      case _ ⇒
    }
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.typeCheck(operand0, contextItemType)
    operand1 = visitor.typeCheck(operand1, contextItemType)
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 0)
    operand0 = TypeChecker.staticTypeCheck(operand0, SequenceType.NODE_SEQUENCE, backwardsCompatible = false, role0)
    val role1 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 1)
    operand1 = TypeChecker.staticTypeCheck(operand1, SequenceType.NODE_SEQUENCE, backwardsCompatible = false, role1)
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
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    if (operator == Token.UNION && operand0.isInstanceOf[AxisExpression] && 
      operand1.isInstanceOf[AxisExpression]) {
      val a0 = operand0.asInstanceOf[AxisExpression]
      val a1 = operand1.asInstanceOf[AxisExpression]
      if (a0.getAxis == Axis.ATTRIBUTE && a1.getAxis == Axis.CHILD) {
        val b = new Block()
        b.setChildren(Array(operand0, operand1))
        return b
      } else if (a1.getAxis == Axis.ATTRIBUTE && a0.getAxis == Axis.CHILD) {
        val b = new Block()
        b.setChildren(Array(operand1, operand0))
        return b
      }
    }
    this
  }

  /**
   * Iterate over the value of the expression. The result will always be sorted in document order,
   * with duplicates eliminated
   * @param c The context for evaluation
   * @return a SequenceIterator representing the union of the two operands
   */
  override def iterate(c: XPathContext): SequenceIterator = {
    var i1 = operand0.iterate(c)
    if ((operand0.getSpecialProperties & StaticProperty.ORDERED_NODESET) == 
      0) {
      i1 = new DocumentOrderIterator(i1, GlobalOrderComparer.getInstance)
    }
    var i2 = operand1.iterate(c)
    if ((operand1.getSpecialProperties & StaticProperty.ORDERED_NODESET) == 
      0) {
      i2 = new DocumentOrderIterator(i2, GlobalOrderComparer.getInstance)
    }
    new VennIterator(i1, i2, GlobalOrderComparer.getInstance, operator)
  }

  /**
   * Get the effective boolean value. In the case of a union expression, this
   * is reduced to an OR expression, for efficiency
   */
  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    if (operator == Token.UNION) {
      operand0.effectiveBooleanValue(context) || operand1.effectiveBooleanValue(context)
    } else {
      super.effectiveBooleanValue(context)
    }
  }
}

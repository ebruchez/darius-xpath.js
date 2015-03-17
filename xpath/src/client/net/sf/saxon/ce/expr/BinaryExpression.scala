package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.Cardinality
import java.util.ArrayList
import java.util.Iterator
import java.util.List
import BinaryExpression._
//remove if not needed
import scala.collection.JavaConversions._

object BinaryExpression {

  /**
   * Determine whether a binary operator is commutative, that is, A op B = B op A.
   * @param operator the operator, for example {@link Token#PLUS}
   * @return true if the operator is commutative
   */
  protected def isCommutative(operator: Int): Boolean = {
    (operator == Token.AND || operator == Token.OR || operator == Token.UNION || 
      operator == Token.INTERSECT || 
      operator == Token.PLUS || 
      operator == Token.MULT || 
      operator == Token.EQUALS || 
      operator == Token.FEQ || 
      operator == Token.NE || 
      operator == Token.FNE)
  }

  /**
   * Determine whether an operator is associative, that is, ((a^b)^c) = (a^(b^c))
   * @param operator the operator, for example {@link Token#PLUS}
   * @return true if the operator is associative
   */
  protected def isAssociative(operator: Int): Boolean = {
    (operator == Token.AND || operator == Token.OR || operator == Token.UNION || 
      operator == Token.INTERSECT || 
      operator == Token.PLUS || 
      operator == Token.MULT)
  }

  /**
   * Test if one operator is the inverse of another, so that (A op1 B) is
   * equivalent to (B op2 A). Commutative operators are the inverse of themselves
   * and are therefore not listed here.
   * @param op1 the first operator
   * @param op2 the second operator
   * @return true if the operators are the inverse of each other
   */
  protected def isInverse(op1: Int, op2: Int): Boolean = op1 != op2 && op1 == Token.inverse(op2)
}

/**
 * Binary Expression: a numeric or boolean expression consisting of the
 * two operands and an operator
 */
abstract class BinaryExpression(protected var operand0: Expression, protected var operator: Int, protected var operand1: Expression)
    extends Expression {

  adoptChildExpression(p0)

  adoptChildExpression(p1)

  /**
   * Simplify an expression
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    operand0 = visitor.simplify(operand0)
    operand1 = visitor.simplify(operand1)
    this
  }

  /**
   * Type-check the expression. Default implementation for binary operators that accept
   * any kind of operand
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.typeCheck(operand0, contextItemType)
    operand1 = visitor.typeCheck(operand1, contextItemType)
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        {@link client.net.sf.saxon.ce.type.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.optimize(operand0, contextItemType)
    operand1 = visitor.optimize(operand1, contextItemType)
    try {
      if ((operand0.isInstanceOf[Literal]) && (operand1.isInstanceOf[Literal])) {
        val v = evaluateItem(new EarlyEvaluationContext(visitor.getConfiguration))
        return Literal.makeLiteral(v)
      }
    } catch {
      case err: XPathException => 
    }
    this
  }

  /**
   * Mark an expression as being "flattened". This is a collective term that includes extracting the
   * string value or typed value, or operations such as simple value construction that concatenate text
   * nodes before atomizing. The implication of all of these is that although the expression might
   * return nodes, the identity of the nodes has no significance. This is called during type checking
   * of the parent expression.
   *
   * @param flattened set to true if the result of the expression is atomized or otherwise turned into
   *                  an atomic value
   */
  def setFlattened(flattened: Boolean) {
    operand0.setFlattened(flattened)
    operand1.setFlattened(flattened)
  }

  /**
   * Promote this expression if possible
   */
  def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      if (offer.action != PromotionOffer.UNORDERED) {
        operand0 = doPromotion(operand0, offer)
        operand1 = doPromotion(operand1, offer)
      }
      this
    }
  }

  /**
   * Get the immediate subexpressions of this expression
   */
  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(operand0, operand1)

  /**
   * Get the operator
   * @return the operator, for example {@link Token#PLUS}
   */
  def getOperator(): Int = operator

  /**
   * Get the operands
   * @return the two operands of the binary expression, as an array of length 2
   */
  def getOperands(): Array[Expression] = Array(operand0, operand1)

  /**
   * Determine the static cardinality. Default implementation returns [0..1] if either operand
   * can be empty, or [1..1] otherwise.
   */
  def computeCardinality(): Int = {
    if (Cardinality.allowsZero(operand0.getCardinality) || Cardinality.allowsZero(operand1.getCardinality)) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.EXACTLY_ONE
    }
  }

  /**
   * Determine the special properties of this expression
   * @return {@link StaticProperty#NON_CREATIVE}. This is overridden
   * for some subclasses.
   */
  def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[BinaryExpression]) {
      val b = other.asInstanceOf[BinaryExpression]
      if (operator == b.operator) {
        if (operand0 == b.operand0 && operand1 == b.operand1) {
          return true
        }
        if (isCommutative(operator) && operand0 == b.operand1 && operand1 == b.operand0) {
          return true
        }
        if (isAssociative(operator) && 
          pairwiseEqual(flattenExpression(new ArrayList(4)), b.flattenExpression(new ArrayList(4)))) {
          return true
        }
      }
      if (isInverse(operator, b.operator) && operand0 == b.operand1 && 
        operand1 == b.operand0) {
        return true
      }
    }
    false
  }

  /**
   * Flatten an expression with respect to an associative operator: for example
   * the expression (a+b) + (c+d) becomes list(a,b,c,d), with the list in canonical
   * order (sorted by hashCode)
   * @param list a list provided by the caller to contain the result
   * @return the list of expressions
   */
  private def flattenExpression(list: List[_]): List[_] = {
    if (operand0.isInstanceOf[BinaryExpression] && 
      operand0.asInstanceOf[BinaryExpression].operator == operator) {
      operand0.asInstanceOf[BinaryExpression].flattenExpression(list)
    } else {
      val h = operand0.hashCode
      list.add(operand0)
      var i = list.size - 1
      while (i > 0 && h > list.get(i - 1).hashCode) {
        list.set(i, list.get(i - 1))
        list.set(i - 1, operand0)
        i -= 1
      }
    }
    if (operand1.isInstanceOf[BinaryExpression] && 
      operand1.asInstanceOf[BinaryExpression].operator == operator) {
      operand1.asInstanceOf[BinaryExpression].flattenExpression(list)
    } else {
      val h = operand1.hashCode
      list.add(operand1)
      var i = list.size - 1
      while (i > 0 && h > list.get(i - 1).hashCode) {
        list.set(i, list.get(i - 1))
        list.set(i - 1, operand1)
        i -= 1
      }
    }
    list
  }

  /**
   * Compare whether two lists of expressions are pairwise equal
   * @param a the first list of expressions
   * @param b the second list of expressions
   * @return true if the two lists are equal
   */
  private def pairwiseEqual(a: List[_], b: List[_]): Boolean = {
    if (a.size != b.size) {
      return false
    }
    for (i <- 0 until a.size if a.get(i) != b.get(i)) {
      return false
    }
    true
  }

  /**
   * Get a hashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = {
    val op = Math.min(operator, Token.inverse(operator))
    ("BinaryExpression " + op).hashCode ^ operand0.hashCode ^ 
      operand1.hashCode
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = {
    "(" + operand0.toString + " " + displayOperator() + " " + 
      operand1.toString + 
      ")"
  }

  /**
   * Display the operator used by this binary expression
   * @return String representation of the operator (for diagnostic display only)
   */
  protected def displayOperator(): String = Token.tokens(operator)
}

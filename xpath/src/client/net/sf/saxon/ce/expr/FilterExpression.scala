// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import java.math.BigDecimal

import client.net.sf.saxon.ce.`type`.{AnyItemType, AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr.FilterExpression._
import client.net.sf.saxon.ce.expr.instruct.Choose
import client.net.sf.saxon.ce.functions._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.{Sequence, SequenceIterator, StructuredQName}
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.value._

import scala.beans.BeanProperty

object FilterExpression {

  private def tryToRewritePositionalFilterSupport(start: Expression, 
      comparand: Expression, 
      operator: Int, 
      th: TypeHierarchy): Expression = {
    if (th.isSubType(comparand.getItemType, AtomicType.INTEGER)) operator match {
      case Token.FEQ => {
        if (Literal.isConstantOne(comparand)) {
          new FirstItemExpression(start)
        } else {
          SystemFunction.makeSystemFunction("subsequence", Array(start, comparand, new Literal(IntegerValue.PLUS_ONE)))
        }
      }
      case Token.FLT => {
        val args = new Array[Expression](3)
        args(0) = start
        args(1) = new Literal(new IntegerValue(1))
        if (Literal.isAtomic(comparand)) {
          val n = comparand.asInstanceOf[Literal].getValue.asInstanceOf[NumericValue]
            .intValue()
          args(2) = new Literal(new IntegerValue(new BigDecimal(n - 1)))
        } else {
          args(2) = new ArithmeticExpression(comparand, Token.MINUS, new Literal(new IntegerValue(1)))
        }
        SystemFunction.makeSystemFunction("subsequence", args)
      }
      case Token.FLE => {
        val args = new Array[Expression](3)
        args(0) = start
        args(1) = new Literal(new IntegerValue(1))
        args(2) = comparand
        SystemFunction.makeSystemFunction("subsequence", args)
      }
      case Token.FNE => {
        SystemFunction.makeSystemFunction("remove", Array(start, comparand))
      }
      case Token.FGT => {
        val args = new Array[Expression](2)
        args(0) = start
        if (Literal.isAtomic(comparand)) {
          val n = comparand.asInstanceOf[Literal].getValue.asInstanceOf[NumericValue]
            .intValue()
          args(1) = new Literal(new IntegerValue(new BigDecimal(n + 1)))
        } else {
          args(1) = new ArithmeticExpression(comparand, Token.PLUS, new Literal(new IntegerValue(1)))
        }
        SystemFunction.makeSystemFunction("subsequence", args)
      }
      case Token.FGE => {
        SystemFunction.makeSystemFunction("subsequence", Array(start, comparand))
      }
      case _ => throw new IllegalArgumentException("operator")
    } else {
      null
    }
  }

  /**
   * Determine whether an expression, when used as a filter, is potentially positional;
   * that is, where it either contains a call on position() or last(), or where it is capable of returning
   * a numeric result.
   *
   * @param exp the expression to be examined
   * @param th  the type hierarchy cache
   * @return true if the expression depends on position() or last() explicitly or implicitly
   */
  private def isPositionalFilter(exp: Expression, th: TypeHierarchy): Boolean = {
    val `type` = exp.getItemType
    if (`type` == AtomicType.BOOLEAN) {
      return isExplicitlyPositional(exp)
    }
    (`type` == AtomicType.ANY_ATOMIC || `type`.isInstanceOf[AnyItemType] || 
      `type` == AtomicType.INTEGER || 
      `type` == AtomicType.NUMERIC || 
      th.isSubType(`type`, AtomicType.NUMERIC) || 
      isExplicitlyPositional(exp))
  }

  /**
   * Determine whether an expression, when used as a filter, has an explicit dependency on position() or last()
   *
   * @param exp the expression being tested
   * @return true if the expression is explicitly positional, that is, if it contains an explicit call on
   *         position() or last()
   */
  private def isExplicitlyPositional(exp: Expression): Boolean = {
    (exp.getDependencies & 
      (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST)) != 
      0
  }
}

/**
 * A FilterExpression contains a base expression and a filter predicate, which may be an
 * integer expression (positional filter), or a boolean expression (qualifier)
 */
class FilterExpression(var start: Expression, @BeanProperty var filter: Expression)
    extends Expression {

  private var filterIsPositional: Boolean = _

  private var filterIsIndependentNumeric: Boolean = _

  adoptChildExpression(start)

  adoptChildExpression(filter)

  /**
   * Get the data type of the items returned
   *
   * @return an integer representing the data type
   */
  def getItemType(): ItemType = {
    if (filter.isInstanceOf[InstanceOfExpression] && 
      filter.asInstanceOf[InstanceOfExpression].getBaseExpression.isInstanceOf[ContextItemExpression]) {
      return filter.asInstanceOf[InstanceOfExpression].getRequiredItemType
    }
    start.getItemType
  }

  /**
   * Get the underlying expression
   *
   * @return the expression being filtered
   */
  def getControllingExpression(): Expression = start

  /**
   * Get the subexpression that is evaluated in the new context
   * @return the subexpression evaluated in the context set by the controlling expression
   */
  def getControlledExpression(): Expression = filter

  /**
   * Determine if the filter is positional
   *
   * @param th the Type Hierarchy (for cached access to type information)
   * @return true if the value of the filter depends on the position of the item against
   *         which it is evaluated
   */
  def isPositional(th: TypeHierarchy): Boolean = isPositionalFilter(filter, th)

  /**
   * Simplify an expression
   *
   * @param visitor the expression visitor
   * @return the simplified expression
   * @throws XPathException if any failure occurs
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    start = visitor.simplify(start)
    filter = visitor.simplify(filter)
    this
  }

  /**
   * Type-check the expression
   *
   * @param visitor         the expression visitor
   * @param contextItemType the type of the context item for this expression
   * @return the expression after type-checking (potentially modified to add run-time
   *         checks and/or conversions)
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    start = visitor.typeCheck(start, contextItemType)
    adoptChildExpression(start)
    val filter2 = visitor.typeCheck(filter, start.getItemType)
    if (filter2 != filter) {
      filter = filter2
      adoptChildExpression(filter2)
    }
    if (Literal.isConstantOne(filter)) {
      return new FirstItemExpression(start)
    }
    if (filter.isInstanceOf[Last]) {
      return new LastItemExpression(start)
    }
    filterIsIndependentNumeric = th.isSubType(filter.getItemType, AtomicType.NUMERIC) && 
      (filter.getDependencies & 
      (StaticProperty.DEPENDS_ON_CONTEXT_ITEM | StaticProperty.DEPENDS_ON_POSITION)) == 
      0 && 
      !Cardinality.allowsMany(filter.getCardinality)
    visitor.resetStaticProperties()
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
   * @throws XPathException if an error is discovered during this phase
   *                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val config = visitor.getConfiguration
    val th = TypeHierarchy.getInstance
    val start2 = visitor.optimize(start, contextItemType)
    if (start2 != start) {
      start = start2
      adoptChildExpression(start2)
    }
    var filter2 = filter.optimize(visitor, start.getItemType)
    if (filter2 != filter) {
      filter = filter2
      adoptChildExpression(filter2)
    }
    if (filter.isInstanceOf[Literal] && 
      filter.asInstanceOf[Literal].getValue.isInstanceOf[BooleanValue]) {
      if (filter.asInstanceOf[Literal].getValue.asInstanceOf[BooleanValue]
        .getBooleanValue) {
        return start
      } else {
        return new Literal(EmptySequence.getInstance)
      }
    }
    filterIsPositional = isPositionalFilter(filter, th)
    val subsequence = tryToRewritePositionalFilter(visitor)
    if (subsequence != null) {
      ExpressionTool.copyLocationInfo(this, subsequence)
      return subsequence.simplify(visitor).typeCheck(visitor, contextItemType)
        .optimize(visitor, contextItemType)
    }
    val offer = new PromotionOffer()
    offer.action = PromotionOffer.FOCUS_INDEPENDENT
    offer.promoteDocumentDependent = (start.getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0
    offer.containingExpression = this
    filter2 = doPromotion(filter, offer)
    if (filter2 != filter) {
      filter = filter2
      adoptChildExpression(filter2)
    }
    if (offer.containingExpression.isInstanceOf[LetExpression]) {
      offer.containingExpression = visitor.optimize(offer.containingExpression, contextItemType)
    }
    offer.containingExpression
  }

  /**
   * Attempt to rewrite a filter expression whose predicate is a test of the form
   * [position() op expr] as a call on functions such as subsequence, remove
   * @param visitor the current expression visitor
   * @return the rewritten expression if a rewrite was possible, or null otherwise
   */
  private def tryToRewritePositionalFilter(visitor: ExpressionVisitor): Expression = {
    if (filter.isInstanceOf[Literal]) {
      val `val` = filter.asInstanceOf[Literal].getValue
      if (`val`.isInstanceOf[NumericValue]) {
        if (`val`.asInstanceOf[NumericValue].isWholeNumber) {
          try {
            val lvalue = `val`.asInstanceOf[NumericValue].intValue()
            if (lvalue <= 0) {
              return Literal.makeEmptySequence()
            } else if (lvalue == 1) {
              return new FirstItemExpression(start)
            } else {
              return SystemFunction.makeSystemFunction("subsequence", Array(start, filter, new Literal(IntegerValue.PLUS_ONE)))
            }
          } catch {
            case err: XPathException => return null
          }
        } else {
          return Literal.makeEmptySequence()
        }
      } else {
        return (if (ExpressionTool.effectiveBooleanValue(`val`.iterate())) start else Literal.makeEmptySequence())
      }
    }
    if (filter.isInstanceOf[ComparisonExpression]) {
      val th = TypeHierarchy.getInstance
      val operands = filter.asInstanceOf[ComparisonExpression].getOperands
      var operator = filter.asInstanceOf[ComparisonExpression].getSingletonOperator
      var comparand: Expression = null
      if (operands(0).isInstanceOf[Position] && 
        th.isSubType(operands(1).getItemType, AtomicType.NUMERIC)) {
        comparand = operands(1)
      } else if (operands(1).isInstanceOf[Position] && 
        th.isSubType(operands(0).getItemType, AtomicType.NUMERIC)) {
        comparand = operands(0)
        operator = Token.inverse(operator)
      } else {
        return null
      }
      if (ExpressionTool.dependsOnFocus(comparand)) {
        return null
      }
      val card = comparand.getCardinality
      if (Cardinality.allowsMany(card)) {
        return null
      }
      if (Cardinality.allowsZero(card)) {
        val let = new LetExpression()
        let.setRequiredType(SequenceType.makeSequenceType(comparand.getItemType, card))
        let.setVariableQName(new StructuredQName("pp", NamespaceConstant.SAXON, "pp" + let.hashCode))
        let.setSequence(comparand)
        comparand = new LocalVariableReference(let)
        val existsArg = new LocalVariableReference(let)
        val exists = SystemFunction.makeSystemFunction("exists", Array(existsArg)).asInstanceOf[Exists]
        val rewrite = tryToRewritePositionalFilterSupport(start, comparand, operator, th)
        if (rewrite == null) {
          return null
        }
        val choice = Choose.makeConditional(exists, rewrite)
        let.setAction(choice)
        let
      } else {
        tryToRewritePositionalFilterSupport(start, comparand, operator, th)
      }
    } else if (filter.isInstanceOf[IntegerRangeTest]) {
      val `val` = filter.asInstanceOf[IntegerRangeTest].getValueExpression
      if (!(`val`.isInstanceOf[Position])) {
        return null
      }
      var min = filter.asInstanceOf[IntegerRangeTest].getMinValueExpression
      val max = filter.asInstanceOf[IntegerRangeTest].getMaxValueExpression
      if (ExpressionTool.dependsOnFocus(min)) {
        return null
      }
      if (ExpressionTool.dependsOnFocus(max)) {
        if (max.isInstanceOf[Last]) {
          return SystemFunction.makeSystemFunction("subsequence", Array(start, min))
        } else {
          return null
        }
      }
      val let = new LetExpression()
      let.setRequiredType(SequenceType.SINGLE_INTEGER)
      let.setVariableQName(new StructuredQName("nn", NamespaceConstant.SAXON, "nn" + let.hashCode))
      let.setSequence(min)
      min = new LocalVariableReference(let)
      val min2 = new LocalVariableReference(let)
      val minMinusOne = new ArithmeticExpression(min2, Token.MINUS, new Literal(new IntegerValue(1)))
      val length = new ArithmeticExpression(max, Token.MINUS, minMinusOne)
      val subs = SystemFunction.makeSystemFunction("subsequence", Array(start, min, length)).asInstanceOf[Subsequence]
      let.setAction(subs)
      let
    } else {
      null
    }
  }

  /**
   * Promote this expression if possible
   *
   * @param offer details of the promotion that is possible
   * @param parent
   * @return the promoted expression (or the original expression, unchanged)
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      if (!(offer.action == PromotionOffer.UNORDERED && filterIsPositional)) {
        start = doPromotion(start, offer)
      }
      if (offer.action == PromotionOffer.REPLACE_CURRENT) {
        filter = doPromotion(filter, offer)
      } else {
      }
      this
    }
  }

  /**
   * Get the immediate subexpressions of this expression
   *
   * @return the subexpressions, as an array
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(start, filter)

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   *
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  override def hasLoopingSubexpression(child: Expression): Boolean = child == filter

  /**
   * Get the static cardinality of this expression
   *
   * @return the cardinality. The method attempts to determine the case where the
   *         filter predicate is guaranteed to select at most one item from the sequence being filtered
   */
  def computeCardinality(): Int = {
    if (filter.isInstanceOf[Literal] && 
      filter.asInstanceOf[Literal].getValue.isInstanceOf[NumericValue]) {
      if (filter.asInstanceOf[Literal].getValue.asInstanceOf[NumericValue]
        .compareTo(1) == 
        0 && 
        !Cardinality.allowsZero(start.getCardinality)) {
        return StaticProperty.ALLOWS_ONE
      } else {
        return StaticProperty.ALLOWS_ZERO_OR_ONE
      }
    }
    if (filterIsIndependentNumeric) {
      return StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    if (!Cardinality.allowsMany(start.getCardinality)) {
      return StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-significant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return the static properties of the expression, as a bit-significant value
   */
  override def computeSpecialProperties(): Int = start.getSpecialProperties

  /**
   * Is this expression the same as another expression?
   *
   * @param other the expression to be compared with this one
   * @return true if the two expressions are statically equivalent
   */
  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[FilterExpression]) {
      val f = other.asInstanceOf[FilterExpression]
      return (start == f.start && filter == f.filter)
    }
    false
  }

  /**
   * get HashCode for comparing two expressions
   *
   * @return the hash code
   */
  override def hashCode(): Int = {
    "FilterExpression".hashCode + start.hashCode + filter.hashCode
  }

  /**
   * Iterate over the results, returning them in the correct order
   *
   * @param context the dynamic context for the evaluation
   * @return an iterator over the expression results
   * @throws XPathException if any dynamic error occurs
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    var startExp = start
    var startValue: Sequence = null
    if (startExp.isInstanceOf[Literal]) {
      startValue = startExp.asInstanceOf[Literal].getValue
    } else if (startExp.isInstanceOf[VariableReference]) {
      startValue = startExp.asInstanceOf[VariableReference].evaluateVariable(context)
      startExp = new Literal(startValue)
    }
    if (startValue.isInstanceOf[EmptySequence]) {
      return EmptyIterator.getInstance
    }
    var filterValue: Sequence = null
    if (filter.isInstanceOf[Literal]) {
      filterValue = filter.asInstanceOf[Literal].getValue
    } else if (filter.isInstanceOf[VariableReference]) {
      filterValue = filter.asInstanceOf[VariableReference].evaluateVariable(context)
    }
//ORBEON unused? can't see how SequenceTool can be a NumericValue
//    if (filterValue.isInstanceOf[SequenceTool]) {
//      if (filterValue.isInstanceOf[NumericValue]) {
//        if (filterValue.asInstanceOf[NumericValue].isWholeNumber) {
//          val pos = filterValue.asInstanceOf[NumericValue].intValue()
//          if (startValue != null) {
//            return SingletonIterator.makeIterator(startValue.itemAt(pos - 1))
//          }
//          if (pos >= 1) {
//            val base = startExp.iterate(context)
//            return Subsequence.makeIterator(base, pos, pos)
//          } else {
//            return EmptyIterator.getInstance
//          }
//        } else {
//          return EmptyIterator.getInstance
//        }
//      }
//    }
    val base = startExp.iterate(context)
    new FilterIterator(base, filter, context)
  }

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as XPathContext.VARIABLES and
   * XPathContext.CURRENT_NODE
   *
   * @return the dependencies
   */
  override def computeDependencies(): Int = {
    (start.getDependencies | 
      (filter.getDependencies & 
      (StaticProperty.DEPENDS_ON_XSLT_CONTEXT | StaticProperty.DEPENDS_ON_LOCAL_VARIABLES | 
      StaticProperty.DEPENDS_ON_USER_FUNCTIONS)))
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = {
    "(" + start.toString + "[" + filter.toString + "])"
  }
}

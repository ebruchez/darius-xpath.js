// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.`type`.{ItemType, Type}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.BooleanFn
import client.net.sf.saxon.ce.om.{Item, SequenceIterator, StructuredQName}
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.{BooleanValue, Cardinality, SequenceType}
import client.net.sf.saxon.ce.{LogConfiguration, LogController}

import scala.beans.BeanProperty

object Choose {

  /**
   * Make a simple conditional expression (if (condition) then (thenExp) else (elseExp)
   *
   * @param condition the condition to be tested
   * @param thenExp   the expression to be evaluated if the condition is true
   * @param elseExp   the expression to be evaluated if the condition is false
   * @return the expression
   */
  def makeConditional(condition: Expression, thenExp: Expression, elseExp: Expression): Expression = {
    if (Literal.isEmptySequence(elseExp)) {
      val conditions = Array(condition)
      val actions = Array(thenExp)
      new Choose(conditions, actions)
    } else {
      val conditions = Array(condition, new Literal(BooleanValue.TRUE))
      val actions = Array(thenExp, elseExp)
      new Choose(conditions, actions)
    }
  }

  /**
   * Make a simple conditional expression (if (condition) then (thenExp) else ()
   *
   * @param condition the condition to be tested
   * @param thenExp   the expression to be evaluated if the condition is true
   * @return the expression
   */
  def makeConditional(condition: Expression, thenExp: Expression): Expression = {
    val conditions = Array(condition)
    val actions = Array(thenExp)
    new Choose(conditions, actions)
  }
}

/**
 * Compiled representation of an xsl:choose or xsl:if element in the stylesheet.
 * Also used for typeswitch in XQuery.
 */
class Choose(@BeanProperty var conditions: Array[Expression], @BeanProperty var actions: Array[Expression])
    extends Instruction {

  if (conditions.length != actions.length) {
    throw new IllegalArgumentException("Choose: unequal length arguments")
  }

  for (i <- 0 until conditions.length) {
    adoptChildExpression(conditions(i))
    adoptChildExpression(actions(i))
  }

  private var conditionTests: Array[String] = null

  def setConditionTests(conditionTests: Array[String]): Unit = {
    this.conditionTests = conditionTests
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @param visitor expression visitor object
   * @return the simplified expression
   * @throws XPathException if an error is discovered during expression
   *                        rewriting
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    for (i <- 0 until conditions.length) {
      conditions(i) = visitor.simplify(conditions(i))
      try {
        actions(i) = visitor.simplify(actions(i))
      } catch {
        case err: XPathException => if (err.isTypeError) {
          throw err
        } else {
          actions(i) = new ErrorExpression(err)
        }
      }
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (i <- 0 until conditions.length) {
      conditions(i) = visitor.typeCheck(conditions(i), contextItemType)
      val err = TypeChecker.ebvError(conditions(i))
      if (err != null) {
        err.setLocator(conditions(i).getSourceLocator)
        throw err
      }
    }
    for (i <- 0 until actions.length) {
      try {
        actions(i) = visitor.typeCheck(actions(i), contextItemType)
      } catch {
        case err: XPathException => if (err.isStaticError) {
          throw err
        } else if (err.isTypeError) {
          if (Literal.isEmptySequence(actions(i))) {
            actions(i) = new ErrorExpression(err)
          } else {
            throw err
          }
        } else {
          actions(i) = new ErrorExpression(err)
        }
      }
    }
    this
  }

  /**
   * Determine whether this expression implements its own method for static type checking
   *
   * @return true - this expression has a non-trivial implementation of the staticTypeCheck()
   *         method
   */
  override def implementsStaticTypeCheck(): Boolean = true

  /**
   * Static type checking for conditional expressions is delegated to the expression itself,
   * and is performed separately on each branch of the conditional, so that dynamic checks are
   * added only on those branches where the check is actually required. This also results in a static
   * type error if any branch is incapable of delivering a value of the required type. One reason
   * for this approach is to avoid doing dynamic type checking on a recursive function call as this
   * prevents tail-call optimization being used.
   *
   *
   * @param req                 the required type
   * @param backwardsCompatible true if backwards compatibility mode applies
   * @param role                the role of the expression in relation to the required type
   * @return the expression after type checking (perhaps augmented with dynamic type checking code)
   * @throws XPathException if failures occur, for example if the static type of one branch of the conditional
   *                        is incompatible with the required type
   */
  override def staticTypeCheck(req: SequenceType, backwardsCompatible: Boolean, role: RoleLocator): Expression = {
    for (i <- 0 until actions.length) {
      actions(i) = TypeChecker.staticTypeCheck(actions(i), req, backwardsCompatible, role)
    }
    if (!Literal.isConstantBoolean(conditions(conditions.length - 1), true) && 
      !Cardinality.allowsZero(req.getCardinality)) {
      val cond = if (conditions.length == 1) "the condition is not" else "none of the conditions is"
      val err = new XPathException("Conditional expression: If " + cond + " satisfied, an empty sequence will be returned, " + 
        "but this is not allowed as the " + 
        role.getMessage)
      err.setErrorCode(role.getErrorCode)
      err.setIsTypeError(true)
      throw err
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (i <- 0 until conditions.length) {
      conditions(i) = visitor.optimize(conditions(i), contextItemType)
      val ebv = BooleanFn.rewriteEffectiveBooleanValue(conditions(i), visitor, contextItemType)
      if (ebv != null && ebv != conditions(i)) {
        conditions(i) = ebv
        adoptChildExpression(ebv)
      }
    }
    for (i <- 0 until actions.length) {
      try {
        actions(i) = visitor.optimize(actions(i), contextItemType)
      } catch {
        case err: XPathException => if (err.isTypeError) {
          throw err
        } else {
          actions(i) = new ErrorExpression(err)
        }
      }
    }
    if (actions.length == 0) {
      return Literal.makeEmptySequence()
    }
    this
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered. For instructions this is the process() method.
   */
  override def getImplementationMethod(): Int = {
    var m = Expression.PROCESS_METHOD | Expression.ITERATE_METHOD
    if (!Cardinality.allowsMany(getCardinality)) {
      m |= Expression.EVALUATE_METHOD
    }
    m
  }

  /**
   * Mark tail-recursive calls on functions. For most expressions, this does nothing.
   *
   * @return 0 if no tail call was found; 1 if a tail call on a different function was found;
   *         2 if a tail recursive call was found and if this call accounts for the whole of the value.
   */
  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int = {
    var result = 0
    for (action <- actions) {
      result = Math.max(result, action.markTailFunctionCalls(qName, arity))
    }
    result
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   *
   * @return the static item type of the instruction
   */
  override def getItemType(): ItemType = {
    var `type` = actions(0).getItemType
    for (i <- 1 until actions.length) {
      `type` = Type.getCommonSuperType(`type`, actions(i).getItemType)
    }
    `type`
  }

  /**
   * Compute the cardinality of the sequence returned by evaluating this instruction
   *
   * @return the static cardinality
   */
  override def computeCardinality(): Int = {
    var card = 0
    var includesTrue = false
    for (i <- 0 until actions.length) {
      card = Cardinality.union(card, actions(i).getCardinality)
      if (Literal.isConstantBoolean(conditions(i), true)) {
        includesTrue = true
      }
    }
    if (!includesTrue) {
      card = Cardinality.union(card, StaticProperty.ALLOWS_ZERO)
    }
    card
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  override def computeSpecialProperties(): Int = {
    var props = actions(0).getSpecialProperties
    for (i <- 1 until actions.length) {
      props &= actions(i).getSpecialProperties
    }
    props
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true if any of the "actions" creates new nodes.
   * (Nodes created by the conditions can't contribute to the result).
   */
  override def createsNewNodes(): Boolean = {
    actions.exists(e => (e.getSpecialProperties & StaticProperty.NON_CREATIVE) == 0)
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  override def iterateSubExpressions(): Iterator[Expression] = {
    val all = new Array[Expression](conditions.length * 2)
    System.arraycopy(conditions, 0, all, 0, conditions.length)
    System.arraycopy(actions, 0, all, conditions.length, conditions.length)
    Iterator(all.iterator)
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   *
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  override protected def promoteInst(offer: PromotionOffer): Unit = {
    if (offer.action == PromotionOffer.UNORDERED || offer.action == PromotionOffer.REPLACE_CURRENT) {
      for (i <- 0 until conditions.length) {
        conditions(i) = doPromotion(conditions(i), offer)
      }
      for (i <- 0 until actions.length) {
        actions(i) = doPromotion(actions(i), offer)
      }
    } else {
      conditions(0) = doPromotion(conditions(0), offer)
    }
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   *
   * @return a representation of the expression as a string
   */
  override def toString(): String = {
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    sb.append("if (")
    for (i <- 0 until conditions.length) {
      sb.append(conditions(i).toString)
      sb.append(") then (")
      sb.append(actions(i).toString)
      if (i == conditions.length - 1) {
        sb.append(")")
      } else {
        sb.append(") else if (")
      }
    }
    sb.toString
  }

  /**
   * Process this instruction, that is, choose an xsl:when or xsl:otherwise child
   * and process it.
   *
   * @param context the dynamic context of this transformation
   * @return a TailCall, if the chosen branch ends with a call of call-template or
   *         apply-templates. It is the caller's responsibility to execute such a TailCall.
   *         If there is no TailCall, returns null.
   * @throws XPathException if any non-recoverable dynamic error occurs
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    val i = choose(context)
    if (i >= 0) {
      enterConditionTrace(i)
      var tail: TailCall = null
      if (actions(i).isInstanceOf[TailCallReturner]) {
        tail = actions(i).asInstanceOf[TailCallReturner].processLeavingTail(context)
      } else {
        actions(i).process(context)
        tail = null
      }
      leaveConditionTrace(i)
      return tail
    }
    null
  }

  /**
   * Identify which of the choices to take
   *
   * @param context the dynamic context
   * @return integer the index of the first choice that matches, zero-based; or -1 if none of the choices
   *         matches
   * @throws XPathException if evaluating a condition fails
   */
  private def choose(context: XPathContext): Int = {
    for (i <- 0 until conditions.length) {
      var b: Boolean = false
      try {
        b = conditions(i).effectiveBooleanValue(context)
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(conditions(i).getSourceLocator)
          throw e
        }
      }
      if (b) {
        return i
      }
    }
    -1
  }

  /**
   * Evaluate an expression as a single item. This always returns either a single Item or
   * null (denoting the empty sequence). No conversion is done. This method should not be
   * used unless the static type of the expression is a subtype of "item" or "item?": that is,
   * it should not be called if the expression may return a sequence. There is no guarantee that
   * this condition will be detected.
   *
   * @param context The context in which the expression is to be evaluated
   * @return the node or atomic value that results from evaluating the
   *         expression; or null to indicate that the result is an empty
   *         sequence
   * @throws XPathException if any dynamic error occurs evaluating the
   *                        expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val i = choose(context)
    if (i < 0) null else actions(i).evaluateItem(context)
  }

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation relies on the process() method: it
   * "pushes" the results of the instruction to a sequence in memory, and then
   * iterates over this in-memory sequence.
   * <p/>
   * In principle instructions should implement a pipelined iterate() method that
   * avoids the overhead of intermediate storage.
   *
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *         of the expression
   * @throws XPathException if any dynamic error occurs evaluating the
   *                        expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val i = choose(context)
    if (i < 0) EmptyIterator.getInstance else actions(i).iterate(context)
  }

  private def enterConditionTrace(i: Int): Unit = {
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled() && conditionTests != null) {
//ORBEON XSLT
//      val xlt = LogController.getTraceListener.asInstanceOf[XSLTTraceListener]
//      xlt.enterChooseItem(conditionTests(i))
    }
  }

  private def leaveConditionTrace(i: Int): Unit = {
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled() && conditionTests != null) {
//ORBEON XSLT
//      val xlt = LogController.getTraceListener.asInstanceOf[XSLTTraceListener]
//      xlt.leaveChooseItem(conditionTests(i))
    }
  }
}

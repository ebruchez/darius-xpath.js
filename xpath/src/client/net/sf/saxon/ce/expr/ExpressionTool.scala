// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.{Configuration, List}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.SourceLocator
import client.net.sf.saxon.ce.value.{StringValue, _}

object ExpressionTool {

  val UNDECIDED = -1
  val NO_EVALUATION_NEEDED = 0
  val EVALUATE_VARIABLE = 1
  val RETURN_EMPTY_SEQUENCE = 5
  val CALL_EVALUATE_ITEM = 7
  val ITERATE_AND_MATERIALIZE = 8
  val PROCESS = 9
  val EVALUATE_SUPPLIED_PARAMETER = 14

  /**
   * Parse an XPath expression. This performs the basic analysis of the expression against the
   * grammar, it binds variable references and function calls to variable definitions and
   * function definitions, and it performs context-independent expression rewriting for
   * optimization purposes.
   *
   * @param expression The expression (as a character string)
   * @param env An object giving information about the compile-time
   *     context of the expression
   * @param container
   * @param start position of the first significant character in the expression
   * @param _terminator The token that marks the end of this expression; typically
   * Token.EOF, but may for example be a right curly brace
   * @param locator the source location of the expression for use in diagnostics
   */
  def make(expression: String, 
      env: StaticContext, 
      container: Container, 
      start: Int, 
      _terminator: Int,
      locator: SourceLocator): Expression = {
    var terminator = _terminator
    val parser = new ExpressionParser()
    parser.setDefaultContainer(container)
    if (terminator == -1) {
      terminator = Token.EOF
    }
    var exp = parser.parse(expression, start, terminator, env)
    exp = ExpressionVisitor.make(env, exp.getExecutable).simplify(exp)
    exp.setSourceLocator(locator)
    exp
  }

  /**
   * Copy location information (the line number and reference to the container) from one expression
   * to another
   * @param from the expression containing the location information
   * @param to the expression to which the information is to be copied
   */
  def copyLocationInfo(from: Expression, to: Expression): Unit = {
    if (from != null && to != null) {
      if (from.sourceLocator != null) {
        to.setSourceLocator(from.sourceLocator)
        to.setContainer(from.getContainer)
      }
    }
  }

  /**
   * Remove unwanted sorting from an expression, at compile time
   * @param config the expression optimizer
   * @param exp the expression to be optimized
   * @param retainAllNodes true if there is a need to retain exactly those nodes returned by exp
   * even if there are duplicates; false if the caller doesn't mind whether duplicate nodes
   * are retained or eliminated
   * @return the expression after rewriting
   */
  def unsorted(config: Configuration, exp: Expression, retainAllNodes: Boolean): Expression = {
    if (exp.isInstanceOf[Literal]) {
      return exp
    }
    val offer = new PromotionOffer()
    offer.action = PromotionOffer.UNORDERED
    offer.retainAllNodes = retainAllNodes
    exp.promote(offer, null)
  }

  /**
   * Determine the method of evaluation to be used when lazy evaluation of an expression is
   * preferred. This method is called at compile time, after all optimizations have been done,
   * to determine the preferred strategy for lazy evaluation, depending on the type of expression.
   *
   * @param exp the expression to be evaluated
   * @return an integer constant identifying the evaluation mode
   */
  def lazyEvaluationMode(exp: Expression): Int = {
    if (exp.isInstanceOf[Literal]) {
      NO_EVALUATION_NEEDED
    } else if (exp.isInstanceOf[VariableReference]) {
      EVALUATE_VARIABLE
    } else if (exp.isInstanceOf[SuppliedParameterReference]) {
      EVALUATE_SUPPLIED_PARAMETER
    } else if ((exp.getDependencies & 
      (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST | 
      StaticProperty.DEPENDS_ON_CURRENT_ITEM | 
      StaticProperty.DEPENDS_ON_CURRENT_GROUP | 
      StaticProperty.DEPENDS_ON_REGEX_GROUP)) != 
      0) {
      eagerEvaluationMode(exp)
    } else if (exp.isInstanceOf[ErrorExpression]) {
      CALL_EVALUATE_ITEM
    } else {
      eagerEvaluationMode(exp)
    }
  }

  /**
   * Determine the method of evaluation to be used when lazy evaluation of an expression is
   * preferred. This method is called at compile time, after all optimizations have been done,
   * to determine the preferred strategy for lazy evaluation, depending on the type of expression.
   *
   * @param exp the expression to be evaluated
   * @return an integer constant identifying the evaluation mode
   */
  def eagerEvaluationMode(exp: Expression): Int = {
    if (exp.isInstanceOf[Literal]) {
      return NO_EVALUATION_NEEDED
    }
    if (exp.isInstanceOf[VariableReference]) {
      return EVALUATE_VARIABLE
    }
    val m = exp.getImplementationMethod
    if ((m & Expression.EVALUATE_METHOD) != 0) {
      CALL_EVALUATE_ITEM
    } else if ((m & Expression.ITERATE_METHOD) != 0) {
      ITERATE_AND_MATERIALIZE
    } else {
      PROCESS
    }
  }

  /**
   * Do lazy evaluation of an expression. This will return a value, which may optionally
   * be a SequenceIntent, which is a wrapper around an iterator over the value of the expression.
   *
   * @param exp the expression to be evaluated
   * @param evaluationMode the evaluation mode for this expression
   * @param context the run-time evaluation context for the expression. If
   *     the expression is not evaluated immediately, then parts of the
   *     context on which the expression depends need to be saved as part of
   *      the Closure
   * @throws XPathException if any error occurs in evaluating the
   *     expression
   * @return a value: either the actual value obtained by evaluating the
   *     expression, or a Closure containing all the information needed to
   *     evaluate it later
   */
  def evaluate(exp: Expression, evaluationMode: Int, context: XPathContext): Sequence = evaluationMode match {
    case NO_EVALUATION_NEEDED ⇒
      exp.asInstanceOf[Literal].getValue
    case EVALUATE_VARIABLE ⇒
      exp.asInstanceOf[VariableReference].evaluateVariable(context)
    case EVALUATE_SUPPLIED_PARAMETER ⇒
      exp.asInstanceOf[SuppliedParameterReference].evaluateVariable(context)
    case RETURN_EMPTY_SEQUENCE ⇒
      EmptySequence.getInstance
    case CALL_EVALUATE_ITEM ⇒
      val item = exp.evaluateItem(context)
      if (item == null) {
        EmptySequence.getInstance
      } else {
        item
      }
    case UNDECIDED | ITERATE_AND_MATERIALIZE ⇒
      SequenceExtent.makeSequenceExtent(exp.iterate(context))
    case PROCESS ⇒
      ??? //ORBEON
      /*
      val controller = context.getController
      val c2 = context.newMinorContext()
      val seq = controller.allocateSequenceOutputter(20)
      val pipe = controller.makePipelineConfiguration()
      seq.setPipelineConfiguration(pipe)
      c2.setTemporaryReceiver(seq)
      seq.open()
      exp.process(c2)
      seq.close()
      val `val` = seq.getSequence
      seq.reset()
      `val`*/
    case _ ⇒
      throw new IllegalArgumentException("Unknown evaluation mode " + evaluationMode)
  }

  /**
   * Scan an expression to find and mark any recursive tail function calls
   * @param exp the expression to be analyzed
   * @param qName the name of the containing function
   * @param arity the arity of the containing function
   * @return 0 if no tail call was found; 1 if a tail call to a different function was found;
   * 2 if a tail call to the specified function was found. In this case the
   * UserFunctionCall object representing the tail function call will also have been marked as
   * a tail call.
   */
  def markTailFunctionCalls(exp: Expression, qName: StructuredQName, arity: Int): Int = exp.markTailFunctionCalls(qName, arity)

  /**
   * Allocate slot numbers to range variables
   *
   * @param exp the expression whose range variables need to have slot numbers assigned
   * @param _nextFree the next slot number that is available for allocation
   * @return the next unallocated slot number.
   */
  def allocateSlots(exp: Expression, _nextFree: Int): Int = {
    var nextFree = _nextFree
    if (exp.isInstanceOf[Assignation]) {
      exp.asInstanceOf[Assignation].setSlotNumber(nextFree)
      nextFree += 1
    }
    if (exp.isInstanceOf[VariableReference]) {
      val varRef = exp.asInstanceOf[VariableReference]
      val binding = varRef.getBinding
      if (exp.isInstanceOf[LocalVariableReference]) {
        varRef.asInstanceOf[LocalVariableReference].setSlotNumber(binding.getLocalSlotNumber)
      }
      if (binding.isInstanceOf[Assignation] && binding.getLocalSlotNumber < 0) {
        val decl = binding.asInstanceOf[Assignation]
        val msg = "*** Internal Saxon error: local variable " + decl.getVariableName + 
          " encountered whose binding has been deleted"
        throw new IllegalStateException(msg)
      }
    }
    val children = exp.iterateSubExpressions()
    while (children.hasNext) {
      val child = children.next()
      nextFree = allocateSlots(child, nextFree)
    }
    nextFree
  }

  /**
   * Determine the effective boolean value of a sequence, given an iterator over the sequence
   * @param iterator An iterator over the sequence whose effective boolean value is required
   * @return the effective boolean value
   * @throws XPathException if a dynamic error occurs
   */
  def effectiveBooleanValue(iterator: SequenceIterator): Boolean = {
    val first = iterator.next()
    if (first == null) {
      return false
    }
    if (first.isInstanceOf[NodeInfo]) {
      true
    } else {
      if (iterator.next() != null) {
        ebvError("a sequence of two or more atomic values")
      }
      if (first.isInstanceOf[BooleanValue]) {
        first.asInstanceOf[BooleanValue].getBooleanValue
      } else if (first.isInstanceOf[StringValue]) {
        !first.asInstanceOf[StringValue].isZeroLength
      } else if (first.isInstanceOf[NumericValue]) {
        first.asInstanceOf[NumericValue].effectiveBooleanValue()
      } else {
        ebvError("a sequence starting with an atomic value other than a boolean, number, string, or URI")
        false
      }
    }
  }

  /**
   * Report an error in computing the effective boolean value of an expression
   * @param reason the nature of the error
   * @throws XPathException
   */
  def ebvError(reason: String): Unit = {
    val err = new XPathException("Effective boolean value is not defined for " + reason)
    err.setErrorCode("FORG0006")
    err.setIsTypeError(true)
    throw err
  }

  /**
   * Ask whether an expression has a dependency on the focus
   * @param exp the expression
   * @return true if the value of the expression depends on the context item, position, or size
   */
  def dependsOnFocus(exp: Expression): Boolean = {
    (exp.getDependencies & StaticProperty.DEPENDS_ON_FOCUS) !=
      0
  }

  /**
   * Determine whether an expression depends on any one of a set of variables
   * @param e the expression being tested
   * @param bindingList the set of variables being tested
   * @return true if the expression depends on one of the given variables
   */
  def dependsOnVariable(e: Expression, bindingList: Array[Binding]): Boolean = {
    if (bindingList == null || bindingList.length == 0) {
      return false
    }
    if (e.isInstanceOf[VariableReference]) {
      (0 until bindingList.length).find(e.asInstanceOf[VariableReference].getBinding == bindingList(_))
        .map(_ ⇒ true)
        .getOrElse(false)
    } else {
      val children = e.iterateSubExpressions()
      while (children.hasNext) {
        val child = children.next()
        if (dependsOnVariable(child, bindingList)) {
          return true
        }
      }
      false
    }
  }

  /**
   * Determine whether an expression contains a call on the function with a given name
   * @param exp The expression being tested
   * @param qName The name of the function
   * @return true if the expression contains a call on the function
   */
  def callsFunction(exp: Expression, qName: StructuredQName): Boolean = {
    if (exp.isInstanceOf[FunctionCall] && 
      (exp.asInstanceOf[FunctionCall].getFunctionName == qName)) {
      return true
    }
    val iter = exp.iterateSubExpressions()
    while (iter.hasNext) {
      val e = iter.next()
      if (callsFunction(e, qName)) {
        return true
      }
    }
    false
  }

//ORBEON
//  /**
//   * Resolve calls to the XSLT current() function within an expression
//   *
//   * @param _exp the expression within which calls to current() should be resolved
//   * @return the expression after resolving calls to current()
//   */
//  def resolveCallsToCurrentFunction(_exp: Expression): Expression = {
//    var exp = _exp
//    if (callsFunction(exp, Current.FN_CURRENT)) {
//      val let = new LetExpression()
//      let.setVariableQName(new StructuredQName("saxon", NamespaceConstant.SAXON, "current" + exp.hashCode))
//      let.setRequiredType(SequenceType.SINGLE_ITEM)
//      let.setSequence(new CurrentItemExpression())
//      val offer = new PromotionOffer()
//      offer.action = PromotionOffer.REPLACE_CURRENT
//      offer.containingExpression = let
//      exp = exp.promote(offer, null)
//      let.setAction(exp)
//      let
//    } else {
//      exp
//    }
//  }

  /**
   * Get a list of all references to a particular variable within a subtree
   * @param exp the expression at the root of the subtree
   * @param binding the variable binding whose references are sought
   * @param list a list to be populated with the references to this variable
   */
  def gatherVariableReferences(exp: Expression, binding: Binding, list: List[VariableReference]): Unit = {
    if (exp.isInstanceOf[VariableReference] && 
      exp.asInstanceOf[VariableReference].getBinding == binding) {
      list.add(exp.asInstanceOf[VariableReference])
    } else {
      val iter = exp.iterateSubExpressions()
      while (iter.hasNext) {
        gatherVariableReferences(iter.next(), binding, list)
      }
    }
  }

  /**
   * Rebind all variable references to a binding
   * @param exp the expression whose contained variable references are to be rebound
   * @param oldBinding the old binding for the variable references
   * @param newBinding the new binding to which the variables should be rebound
   */
  def rebindVariableReferences(exp: Expression, oldBinding: Binding, newBinding: Binding): Unit = {
    if (exp.isInstanceOf[VariableReference]) {
      if (exp.asInstanceOf[VariableReference].getBinding == oldBinding) {
        exp.asInstanceOf[VariableReference].fixup(newBinding)
      }
    } else {
      val iter = exp.iterateSubExpressions()
      while (iter.hasNext) {
        val e = iter.next()
        rebindVariableReferences(e, oldBinding, newBinding)
      }
    }
  }
}

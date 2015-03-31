// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trace.Location
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceType
import java.util.Iterator
import UserFunction._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object UserFunction {

  /**
   * Determine whether a given expression contains calls on user-defined functions
   *
   * @param exp the expression to be tested
   * @return true if the expression contains calls to user functions.
   */
  private def containsUserFunctionCalls(exp: Expression): Boolean = {
    if (exp.isInstanceOf[UserFunctionCall]) {
      return true
    }
    val i = exp.iterateSubExpressions()
    while (i.hasNext) {
      val e = i.next().asInstanceOf[Expression]
      if (containsUserFunctionCalls(e)) {
        return true
      }
    }
    false
  }
}

/**
 * This object represents the compiled form of a user-written function
 * (the source can be either an XSLT stylesheet function or an XQuery function).
 * <p/>
 * <p>It is assumed that type-checking, of both the arguments and the results,
 * has been handled at compile time. That is, the expression supplied as the body
 * of the function must be wrapped in code to check or convert the result to the
 * required type, and calls on the function must be wrapped at compile time to check or
 * convert the supplied arguments.
 */
class UserFunction extends Procedure {

  @BeanProperty
  var functionName: StructuredQName = _

  private var tailCalls: Boolean = false

  @BooleanBeanProperty
  var tailRecursive: Boolean = false

  @BeanProperty
  var parameterDefinitions: Array[UserFunctionParameter] = _

  private var resultType: SequenceType = _

  protected var evaluationMode: Int = ExpressionTool.UNDECIDED

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   */
  def getObjectName(): StructuredQName = functionName

  /**
   * Determine the preferred evaluation mode for this function
   */
  def computeEvaluationMode(): Unit = {
    evaluationMode = if (tailRecursive) ExpressionTool.eagerEvaluationMode(getBody) else ExpressionTool.lazyEvaluationMode(getBody)
  }

  /**
   * Set the declared result type of the function
   *
   * @param resultType the declared return type
   */
  def setResultType(resultType: SequenceType): Unit = {
    this.resultType = resultType
  }

  /**
   * Indicate whether the function contains a tail call
   *
   * @param tailCalls          true if the function contains a tail call (on any function)
   * @param recursiveTailCalls true if the function contains a tail call (on itself)
   */
  def setTailRecursive(tailCalls: Boolean, recursiveTailCalls: Boolean): Unit = {
    this.tailCalls = tailCalls
    tailRecursive = recursiveTailCalls
  }

  /**
   * Determine whether the function contains tail calls (on this or other functions)
   *
   * @return true if the function contains tail calls
   */
  def containsTailCalls(): Boolean = tailCalls

  /**
   * Get the type of value returned by this function
   *
   * @return the declared result type, or the inferred result type
   *         if this is more precise
   */
  def getResultType(): SequenceType = {
    if (resultType == SequenceType.ANY_SEQUENCE) {
      if (!containsUserFunctionCalls(getBody)) {
        resultType = SequenceType.makeSequenceType(getBody.getItemType, getBody.getCardinality)
      }
    }
    resultType
  }

  /**
   * Get the declared result type
   *
   * @return the declared result type
   */
  def getDeclaredResultType(): SequenceType = resultType

  /**
   * Get the required types of an argument to this function
   *
   * @param n identifies the argument in question, starting at 0
   * @return a SequenceType object, indicating the required type of the argument
   */
  def getArgumentType(n: Int): SequenceType = parameterDefinitions(n).getRequiredType

  /**
   * Get the evaluation mode. The evaluation mode will be computed if this has not already been done
   *
   * @return the computed evaluation mode
   */
  def getEvaluationMode(): Int = {
    if (evaluationMode == ExpressionTool.UNDECIDED) {
      computeEvaluationMode()
    }
    evaluationMode
  }

  /**
   * Get the arity of this function
   *
   * @return the number of arguments
   */
  def getNumberOfArguments(): Int = parameterDefinitions.length

  /**
   * Call this function to return a value.
   *
   * @param actualArgs the arguments supplied to the function. These must have the correct
   *                   types required by the function signature (it is the caller's responsibility to check this).
   *                   The array must be the correct size to match
   *                   the number of arguments: again, it is the caller's responsibility to check this.
   * @param context    This provides the run-time context for evaluating the function. It is the caller's
   *                   responsibility to allocate a "clean" context for the function to use; the context that is provided
   *                   will be overwritten by the function.
   * @return a Value representing the result of the function.
   */
  def call(actualArgs: Array[Sequence], context: XPathContext): Sequence = {
    if (evaluationMode == ExpressionTool.UNDECIDED) {
      computeEvaluationMode()
    }
    context.setStackFrame(getNumberOfSlots, actualArgs)
    var result: Sequence = null
    try {
      result = ExpressionTool.evaluate(getBody, evaluationMode, context)
    } catch {
      case err: XPathException ⇒ {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
    result
  }

  /**
   * Call this function in "push" mode, writing the results to the current output destination.
   *
   * @param actualArgs the arguments supplied to the function. These must have the correct
   *                   types required by the function signature (it is the caller's responsibility to check this).
   *                   The array must be the correct size to match
   *                   the number of arguments: again, it is the caller's responsibility to check this.
   * @param context    This provides the run-time context for evaluating the function. It is the caller's
   *                   responsibility to allocate a "clean" context for the function to use; the context that is provided
   *                   will be overwritten by the function.
   */
  def process(actualArgs: Array[Sequence], context: XPathContext): Unit = {
    context.setStackFrame(getNumberOfSlots, actualArgs)
    getBody.process(context)
  }

  def getConstructType(): StructuredQName = Location.FUNCTION
}

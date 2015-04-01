// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr.instruct.UserFunction
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value._

import scala.collection.JavaConversions._

/**
 * This class represents a call to a user-defined function in the stylesheet or query.
 */
class UserFunctionCall extends FunctionCall {

  private var staticType: SequenceType = _

  private var function: UserFunction = _

  private var tailCall: Boolean = false

  /**
   * Set the static type
   *
   * @param type the static type of the result of the function call
   */
  def setStaticType(`type`: SequenceType): Unit = {
    staticType = `type`
  }

  /**
   * Create the reference to the function to be called
   *
   * @param compiledFunction the function being called
   */
  def setFunction(compiledFunction: UserFunction): Unit = {
    function = compiledFunction
  }

  /**
   * Check the function call against the declared function signature
   *
   * @param compiledFunction the function being called
   * @param visitor          an expression visitor
   */
  def checkFunctionCall(compiledFunction: UserFunction, visitor: ExpressionVisitor): Unit = {
    val n = compiledFunction.getNumberOfArguments
    for (i ← 0 until n) {
      val role = new RoleLocator(RoleLocator.FUNCTION, compiledFunction.getFunctionName, i)
      role.setErrorCode("XTTE0790")
      argument(i) = TypeChecker.staticTypeCheck(argument(i), compiledFunction.getArgumentType(i), backwardsCompatible = false,
        role)
    }
  }

  /**
   * Method called during the type checking phase
   */
  def checkArguments(visitor: ExpressionVisitor): Unit = {
  }

  /**
   * Get the qualified of the function being called
   *
   * @return the qualified name
   */
  def getFunctionName: StructuredQName = {
    val n = super.getFunctionName
    if (n == null) {
      function.getFunctionName
    } else {
      n
    }
  }

  /**
   * Pre-evaluate a function at compile time. This version of the method suppresses
   * early evaluation by doing nothing.
   *
   * @param visitor an expression visitor
   */
  def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Determine the data type of the expression, if possible
   *
   * @return Type.ITEM (meaning not known in advance)
   */
  def getItemType: ItemType = {
    if (staticType == null) {
      AnyItemType.getInstance
    } else {
      staticType.getPrimaryType
    }
  }

  def getIntrinsicDependencies: Int = {
    StaticProperty.DEPENDS_ON_USER_FUNCTIONS
  }

  /**
   * Determine the cardinality of the result
   */
  def computeCardinality(): Int = {
    if (staticType == null) {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    } else {
      staticType.getCardinality
    }
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.typeCheck(visitor, contextItemType)
    if (function != null) {
      if (staticType == SequenceType.ANY_SEQUENCE) {
        staticType = function.getResultType
      }
    }
    e
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
        for (i ← 0 until argument.length) {
          argument(i) = doPromotion(argument(i), offer)
        }
      }
      this
    }
  }

  /**
   * Mark tail-recursive calls on stylesheet functions. This marks the function call as tailRecursive if
   * if is a call to the containing function, and in this case it also returns "true" to the caller to indicate
   * that a tail call was found.
   */
  def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int = {
    tailCall = true
    if (getFunctionName == qName && arity == getNumberOfArguments) 2 else 1
  }

  def getImplementationMethod: Int = {
    if (Cardinality.allowsMany(getCardinality)) {
      ITERATE_METHOD | PROCESS_METHOD
    } else {
      EVALUATE_METHOD
    }
  }

  /**
   * Call the function, returning the value as an item. This method will be used
   * only when the cardinality is zero or one. If the function is tail recursive,
   * it returns an Object representing the arguments to the next (recursive) call
   */
  def evaluateItem(c: XPathContext): Item = {
    val `val` = callFunction(c)
    SequenceTool.asItem(`val`)
  }

  /**
   * Call the function, returning an iterator over the results. (But if the function is
   * tail recursive, it returns an iterator over the arguments of the recursive call)
   */
  def iterate(c: XPathContext): SequenceIterator = callFunction(c).iterate()

  /**
   * This is the method that actually does the function call
   *
   * @param c the dynamic context
   * @return the result of the function
   * @throws XPathException if dynamic errors occur
   */
  private def callFunction(c: XPathContext): Sequence = {
    val actualArgs = evaluateArguments(c)
    if (tailCall) {
      c.requestTailCall(function, actualArgs)
      return EmptySequence.getInstance
    }
    val c2 = c.newCleanContext()
    c2.setTemporaryOutputState(true)
    function.call(actualArgs, c2)
  }

  /**
   * Process the function call in push mode
   *
   * @param context the XPath dynamic context
   * @throws XPathException
   */
  def process(context: XPathContext): Unit = {
    val actualArgs = evaluateArguments(context)
    if (tailCall) {
      context.requestTailCall(function, actualArgs)
    } else {
      val out = context.getReceiver
      val c2 = context.newCleanContext()
      c2.setTemporaryReceiver(out)
      function.process(actualArgs, c2)
    }
  }

  private def evaluateArguments(c: XPathContext): Array[Sequence] = {
    val numArgs = argument.length
    val actualArgs = Array.ofDim[Sequence](numArgs)
    for (i ← 0 until numArgs) {
      actualArgs(i) = SequenceExtent.makeSequenceExtent(argument(i).iterate(c))
    }
    actualArgs
  }

  def getObjectName(): StructuredQName = getFunctionName
}

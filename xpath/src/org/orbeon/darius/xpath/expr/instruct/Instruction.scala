// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.`type`.{ItemType, Type}
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.SourceLocator

object Instruction {

  /**
   * Construct an exception with diagnostic information. Note that this method
   * returns the exception, it does not throw it: that is up to the caller.
   * @param loc the location of the error
   * @param error The exception containing information about the error
   * @param context The controller of the transformation
   * @return an exception based on the supplied exception, but with location information
   * added relating to this instruction
   */
  protected def dynamicError(loc: SourceLocator, error: XPathException, context: XPathContext): XPathException = {
    if (error.isInstanceOf[TerminationException]) {
      return error
    }
    error.maybeSetLocation(loc)
    error
  }

//ORBEON XSLT
//  /**
//   * Assemble a ParameterSet. Method used by instructions that have xsl:with-param
//   * children. This method is used for the non-tunnel parameters.
//   * @param context the XPath dynamic context
//   * @param actualParams the set of with-param parameters that specify tunnel="no"
//   * @return a ParameterSet
//   */
//  def assembleParams(context: XPathContext, actualParams: Array[WithParam]): ParameterSet = {
//    if (actualParams == null || actualParams.length == 0) {
//      return null
//    }
//    val params = new ParameterSet(actualParams.length)
//    for (actualParam ← actualParams) {
//      params.put(actualParam.getParameterId, actualParam.getSelectValue(context), actualParam.isTypeChecked)
//    }
//    params
//  }

//ORBEON XSLT
//  /**
//   * Assemble a ParameterSet. Method used by instructions that have xsl:with-param
//   * children. This method is used for the tunnel parameters.
//   * @param context the XPath dynamic context
//   * @param actualParams the set of with-param parameters that specify tunnel="yes"
//   * @return a ParameterSet
//   */
//  def assembleTunnelParams(context: XPathContext, actualParams: Array[WithParam]): ParameterSet = {
//    val existingParams = context.getTunnelParameters
//    if (existingParams == null) {
//      return assembleParams(context, actualParams)
//    }
//    val newParams = new ParameterSet(existingParams, (if (actualParams == null) 0 else actualParams.length))
//    if (actualParams == null || actualParams.length == 0) {
//      return newParams
//    }
//    for (actualParam ← actualParams) {
//      newParams.put(actualParam.getParameterId, actualParam.getSelectValue(context), false)
//    }
//    newParams
//  }
}

/**
 * Abstract superclass for all instructions in the compiled stylesheet.
 * This represents a compiled instruction, and as such, the minimum information is
 * retained from the original stylesheet. <br>
 * Note: this class implements SourceLocator: that is, it can identify where in the stylesheet
 * the source instruction was located.
 */
abstract class Instruction extends Expression with TailCallReturner {

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered. For instructions this is the process() method.
   */
  override def getImplementationMethod: Int = Expression.PROCESS_METHOD

  /**
   * Get the item type of the items returned by evaluating this instruction
   * @return the static item type of the instruction
   */
  def getItemType: ItemType = Type.ITEM_TYPE

  /**
   * Get the cardinality of the sequence returned by evaluating this instruction
   * @return the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  /**
   * ProcessLeavingTail: called to do the real work of this instruction. This method
   * must be implemented in each subclass. The results of the instruction are written
   * to the current Receiver, which can be obtained via the Controller.
   * @param context The dynamic context of the transformation, giving access to the current node,
   * the current variables, etc.
   * @return null if the instruction has completed execution; or a TailCall indicating
   * a function call or template call that is delegated to the caller, to be made after the stack has
   * been unwound so as to save stack space.
   */
  def processLeavingTail(context: XPathContext): TailCall

  /**
   * Process the instruction, without returning any tail calls
   * @param context The dynamic context, giving access to the current node,
   * the current variables, etc.
   */
  override def process(context: XPathContext): Unit = {
    try {
      var tc = processLeavingTail(context)
      while (tc != null) {
        tc = tc.processLeavingTail()
      }
    } catch {
      case err: XPathException ⇒
        err.maybeSetLocation(getSourceLocator)
        throw err
    }
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   *
   * @throws org.orbeon.darius.xpath.trans.XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    if (createsNewNodes()) {
      p
    } else {
      p | StaticProperty.NON_CREATIVE
    }
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns a default value of false
   * @return true if the instruction creates new nodes (or if it can't be proved that it doesn't)
   */
  def createsNewNodes(): Boolean = false

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
  }

  /**
   * Offer promotion for this subexpression. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *     expressions that don't depend on the context to an outer level in
   *     the containing expression
   * @param parent
   * @throws org.orbeon.darius.xpath.trans.XPathException if any error is detected
   * @return if the offer is not accepted, return this expression unchanged.
   *      Otherwise return the result of rewriting the expression to promote
   *      this subexpression
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      promoteInst(offer)
      this
    }
  }

  /**
   * Evaluate an expression as a single item. This always returns either a single Item or
   * null (denoting the empty sequence). No conversion is done. This method should not be
   * used unless the static type of the expression is a subtype of "item" or "item?": that is,
   * it should not be called if the expression may return a sequence. There is no guarantee that
   * this condition will be detected.
   *
   * @param context The context in which the expression is to be evaluated
   * @throws XPathException if any dynamic error occurs evaluating the
   *     expression
   * @return the node or atomic value that results from evaluating the
   *     expression; or null to indicate that the result is an empty
   *     sequence
   */
  override def evaluateItem(context: XPathContext): Item = {
    val m = getImplementationMethod
    if ((m & Expression.EVALUATE_METHOD) != 0) {
      throw new AssertionError("evaluateItem() is not implemented in the subclass " + 
        getClass)
    } else if ((m & Expression.ITERATE_METHOD) != 0) {
      iterate(context).next()
    } else {
      ???
      //ORBEON XSLT
//      val seq = pushToSequence(1, context)
//      val result = seq.getFirstItem
//      seq.reset()
//      result
    }
  }

  //ORBEON XSLT
//  private def pushToSequence(len: Int, context: XPathContext): SequenceOutputter = {
//    val controller = context.getController
//    val c2 = context.newMinorContext()
//    val seq = controller.allocateSequenceOutputter(len)
//    val pipe = controller.makePipelineConfiguration()
//    seq.setPipelineConfiguration(pipe)
//    c2.setTemporaryReceiver(seq)
//    process(c2)
//    seq.close()
//    seq
//  }

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation handles iteration for expressions that
   * return singleton values: for non-singleton expressions, the subclass must
   * provide its own implementation.
   *
   * @throws XPathException if any dynamic error occurs evaluating the
   *     expression
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *     of the expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val m = getImplementationMethod
    if ((m & Expression.EVALUATE_METHOD) != 0) {
      super.iterate(context)
    } else if ((m & Expression.ITERATE_METHOD) != 0) {
      throw new AssertionError("iterate")
    } else {
      ???
      //ORBEON XSLT
//      val seq = pushToSequence(20, context)
//      seq.iterate()
    }
  }
}

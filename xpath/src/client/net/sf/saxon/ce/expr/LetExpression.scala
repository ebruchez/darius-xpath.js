// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr.instruct.{TailCall, TailCallReturner}
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator, StructuredQName}
import client.net.sf.saxon.ce.orbeon.ArrayList
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceType

import scala.util.control.Breaks._

/**
 * A LetExpression is modelled on the XQuery syntax let $x := expr return expr. This syntax
 * is not available in the surface XPath language, but it is used internally in an optimized
 * expression tree.
 */
class LetExpression extends Assignation with TailCallReturner {

  var evaluationMode: Int = ExpressionTool.UNDECIDED

  /**
   * Set the evaluation mode
   */
  def setEvaluationMode(mode: Int): Unit = {
    evaluationMode = mode
  }

  /**
   * Type-check the expression. This also has the side-effect of counting the number of references
   * to the variable (treating references that occur within a loop specially)
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    sequence = visitor.typeCheck(sequence, contextItemType)
    val role = new RoleLocator(RoleLocator.VARIABLE, getVariableQName, 0)
    val th = TypeHierarchy.getInstance
    val actualItemType = sequence.getItemType
    refineTypeInformation(actualItemType, sequence.getCardinality, sequence match {
      case literal: Literal ⇒ literal.getValue
      case _ ⇒ null
    },
      sequence.getSpecialProperties, visitor, this)
    action = visitor.typeCheck(action, contextItemType)
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
   * Static type checking for let expressions is delegated to the expression itself,
   * and is performed on the "action" expression, to allow further delegation to the branches
   * of a conditional
   *
   * @param req the required type
   * @param backwardsCompatible true if backwards compatibility mode applies
   * @param role the role of the expression in relation to the required type
   * @return the expression after type checking (perhaps augmented with dynamic type checking code)
   * @throws XPathException if failures occur, for example if the static type of one branch of the conditional
   * is incompatible with the required type
   */
  override def staticTypeCheck(req: SequenceType, backwardsCompatible: Boolean, role: RoleLocator): Expression = {
    action = TypeChecker.staticTypeCheck(action, req, backwardsCompatible, role)
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
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val env = visitor.getStaticContext
    action match {
      case reference: VariableReference if reference.getBinding == this ⇒
        return visitor.optimize(sequence, contextItemType)
      case _ ⇒
    }
//ORBEON XSLT
//    if (sequence.isInstanceOf[DocumentInstr] && sequence.asInstanceOf[DocumentInstr].isTextOnly) {
//      if (allReferencesAreFlattened()) {
//        sequence = sequence.asInstanceOf[DocumentInstr].getStringValueExpression(env)
//        requiredType = SequenceType.SINGLE_UNTYPED_ATOMIC
//        adoptChildExpression(sequence)
//      }
//    }
    var tries = 0
    breakable {
      while (tries < 5) {
        tries += 1
        val seq2 = visitor.optimize(sequence, contextItemType)
        if (seq2 == sequence) {
          break()
        }
        sequence = seq2
        adoptChildExpression(sequence)
        visitor.resetStaticProperties()
      }
    }
    tries = 0
    breakable {
      while (tries < 5) {
        tries += 1
        val act2 = visitor.optimize(action, contextItemType)
        if (act2 == action) {
          break()
        }
        action = act2
        adoptChildExpression(action)
        visitor.resetStaticProperties()
      }
    }
    evaluationMode = ExpressionTool.lazyEvaluationMode(sequence)
    this
  }

  /**
   * Determine whether all references to this variable are using the value either
   * (a) by atomizing it, or (b) by taking its string value. (This excludes usages
   * such as testing the existence of a node or taking the effective boolean value).
   * @return true if all references are known to atomize (or stringify) the value,
   * false otherwise. The value false may indicate "not known".
   */
  private def allReferencesAreFlattened(): Boolean = {
    val references = new ArrayList[VariableReference]()
    ExpressionTool.gatherVariableReferences(action, this, references)
    var i = references.size - 1
    while (i >= 0) {
      val bref = references.get(i)
      if (bref.isFlattened) {
      } else {
        return false
      }
      i -= 1
    }
    true
  }

  /**
   * Iterate over the result of the expression to return a sequence of items
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.action match {
          case letExpression: LetExpression ⇒
            let = letExpression
          case _ ⇒
            break()
        }
      }
    }
    let.action.iterate(context)
  }

  /**
   * Evaluate the variable.
   * @param context the dynamic evaluation context
   * @return the result of evaluating the expression that is bound to the variable
   */
  protected def eval(context: XPathContext): Sequence = {
    if (evaluationMode == ExpressionTool.UNDECIDED) {
      evaluationMode = ExpressionTool.lazyEvaluationMode(sequence)
    }
    ExpressionTool.evaluate(sequence, evaluationMode, context)
  }

  /**
   * Evaluate the expression as a singleton
   */
  override def evaluateItem(context: XPathContext): Item = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.action match {
          case letExpression: LetExpression ⇒
            let = letExpression
          case _ ⇒
            break()
        }
      }
    }
    let.action.evaluateItem(context)
  }

  /**
   * Process this expression as an instruction, writing results to the current
   * outputter
   */
  override def process(context: XPathContext): Unit = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.action match {
          case letExpression: LetExpression ⇒
            let = letExpression
          case _ ⇒
            break()
        }
      }
    }
    let.action.process(context)
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   *
   * @return one of the values Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   *         or Type.ITEM (meaning not known in advance)
   */
  def getItemType: ItemType = action.getItemType

  /**
   * Determine the static cardinality of the expression
   */
  def computeCardinality(): Int = action.getCardinality

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    var props = action.getSpecialProperties
    val seqProps = sequence.getSpecialProperties
    if ((seqProps & StaticProperty.NON_CREATIVE) == 0) {
      props &= ~StaticProperty.NON_CREATIVE
    }
    props
  }

  /**
   * Mark tail function calls
   */
  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int = {
    ExpressionTool.markTailFunctionCalls(action, qName, arity)
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      val seq2 = doPromotion(sequence, offer)
      if (seq2 != sequence) {
        sequence = seq2
      }
      if (offer.action == PromotionOffer.UNORDERED || offer.action == PromotionOffer.REPLACE_CURRENT) {
        action = doPromotion(action, offer)
      } else if (offer.action == PromotionOffer.RANGE_INDEPENDENT || offer.action == PromotionOffer.FOCUS_INDEPENDENT) {
        val savedBindingList = offer.bindingList
        offer.bindingList = extendBindingList(offer.bindingList)
        action = doPromotion(action, offer)
        offer.bindingList = savedBindingList
      }
      this
    }
  }

  /**
   * ProcessLeavingTail: called to do the real work of this instruction.
   * The results of the instruction are written
   * to the current Receiver, which can be obtained via the Controller.
   *
   * @param context The dynamic context of the transformation, giving access to the current node,
   *                the current variables, etc.
   * @return null if the instruction has completed execution; or a TailCall indicating
   *         a function call or template call that is delegated to the caller, to be made after the stack has
   *         been unwound so as to save stack space.
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.action match {
          case letExpression: LetExpression ⇒
            let = letExpression
          case _ ⇒
            break()
        }
      }
    }
    let.action match {
      case tailCallReturner: TailCallReturner ⇒
        tailCallReturner.processLeavingTail(context)
      case _ ⇒
        let.action.process(context)
        null
    }
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   * @return a representation of the expression as a string
   */
  override def toString: String = {
    "let $" + getVariableName + " := " + sequence.toString + 
      " return " + 
      action.toString
  }
}

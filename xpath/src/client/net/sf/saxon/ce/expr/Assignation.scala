// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.om.{Sequence, StructuredQName}
import client.net.sf.saxon.ce.orbeon.{ArrayList, Iterator}
import client.net.sf.saxon.ce.value.SequenceType

/**
 * Assignation is an abstract superclass for the kinds of expression
 * that declare range variables: for, some, and every.
 */
abstract class Assignation extends Expression with Binding {

  protected var slotNumber: Int = -999

  protected var sequence: Expression = _

  protected var action: Expression = _

  protected var variableName: StructuredQName = _

  protected var requiredType: SequenceType = _

  /**
   * Set the required type (declared type) of the variable
   * @param requiredType the required type
   */
  def setRequiredType(requiredType: SequenceType) {
    this.requiredType = requiredType
  }

  /**
   * Set the name of the variable
   * @param variableName the name of the variable
   */
  def setVariableQName(variableName: StructuredQName) {
    this.variableName = variableName
  }

  /**
   * Get the name of the variable
   * @return the variable name, as a QName
   */
  def getVariableQName(): StructuredQName = variableName

  def getObjectName(): StructuredQName = variableName

  /**
   * Get the declared type of the variable
   *
   * @return the declared type
   */
  def getRequiredType(): SequenceType = requiredType

  /**
   * If this is a local variable held on the local stack frame, return the corresponding slot number.
   * In other cases, return -1.
   */
  def getLocalSlotNumber(): Int = slotNumber

  /**
   * Get the value of the range variable
   */
  def evaluateVariable(context: XPathContext): Sequence = {
    context.evaluateLocalVariable(slotNumber)
  }

  /**
   * Add the "return" or "satisfies" expression, and fix up all references to the
   * range variable that occur within that expression
   * @param action the expression that occurs after the "return" keyword of a "for"
   * expression, the "satisfies" keyword of "some/every", or the ":=" operator of
   * a "let" expression.
   *
   *
   */
  def setAction(action: Expression) {
    this.action = action
    adoptChildExpression(action)
  }

  /**
   * Indicate whether the binding is local or global. A global binding is one that has a fixed
   * value for the life of a query or transformation; any other binding is local.
   */
  def isGlobal(): Boolean = false

  /**
   * Get the action expression
   * @return the action expression (introduced by "return" or "satisfies")
   */
  def getAction(): Expression = action

  /**
   * Set the "sequence" expression - the one to which the variable is bound
   * @param sequence the expression to which the variable is bound
   */
  def setSequence(sequence: Expression) {
    this.sequence = sequence
    adoptChildExpression(sequence)
  }

  /**
   * Get the "sequence" expression - the one to which the variable is bound
   * @return the expression to which the variable is bound
   */
  def getSequence(): Expression = sequence

  /**
   * Set the slot number for the range variable
   * @param nr the slot number to be used
   */
  def setSlotNumber(nr: Int) {
    slotNumber = nr
  }

  /**
   * Simplify the expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    sequence = visitor.simplify(sequence)
    action = visitor.simplify(action)
    this
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      sequence = doPromotion(sequence, offer)
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
   * Extend an array of variable bindings to include the binding(s) defined in this expression
   * @param in a set of variable bindings
   * @return a set of variable bindings including all those supplied plus this one
   */
  def extendBindingList(in: Array[Binding]): Array[Binding] = {
    var newBindingList: Array[Binding] = null
    if (in == null) {
      newBindingList = new Array[Binding](1)
    } else {
      newBindingList = new Array[Binding](in.length + 1)
      System.arraycopy(in, 0, newBindingList, 0, in.length)
    }
    newBindingList(newBindingList.length - 1) = this
    newBindingList
  }

  /**
   * Get the immediate subexpressions of this expression
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(sequence, action)

  /**
   * Get the display name of the range variable, for diagnostics only
   * @return the lexical QName of the range variable
   */
  def getVariableName(): String = {
    if (variableName == null) {
      "zz:var" + hashCode
    } else {
      variableName.getDisplayName
    }
  }

  /**
   * Refine the type information associated with this variable declaration. This is useful when the
   * type of the variable has not been explicitly declared (which is common); the variable then takes
   * a static type based on the type of the expression to which it is bound. The effect of this call
   * is to update the static expression type for all references to this variable.
   * @param type the inferred item type of the expression to which the variable is bound
   * @param cardinality the inferred cardinality of the expression to which the variable is bound
   * @param constantValue the constant value to which the variable is bound (null if there is no constant value)
   * @param properties other static properties of the expression to which the variable is bound
   * @param visitor an expression visitor to provide context information
   * @param currentExpression the expression that binds the variable
   */
  def refineTypeInformation(`type`: ItemType, 
      cardinality: Int, 
      constantValue: Sequence, 
      properties: Int, 
      visitor: ExpressionVisitor, 
      currentExpression: Assignation) {
    val references = new ArrayList[VariableReference]()
    ExpressionTool.gatherVariableReferences(currentExpression.getAction, this, references)
    for (ref <- references) {
      ref.refineVariableType(`type`, cardinality, constantValue, properties, visitor)
      visitor.resetStaticProperties()
    }
  }
}

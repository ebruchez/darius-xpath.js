// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.value._

/**
 * A ItemChecker implements the item type checking of "treat as": that is,
 * it returns the supplied sequence, checking that all its items are of the correct type
 */
class ItemChecker(sequence: Expression, var requiredItemType: ItemType, var role: RoleLocator)
    extends UnaryExpression(sequence) {

  adoptChildExpression(sequence)

  /**
   * Get the required type
   * @return the required type of the items in the sequence
   */
  def getRequiredType(): ItemType = requiredItemType

  /**
   * Simplify an expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    if (requiredItemType.isInstanceOf[AnyItemType]) {
      return operand
    }
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    val th = TypeHierarchy.getInstance
    val card = operand.getCardinality
    if (card == StaticProperty.EMPTY) {
      return operand
    }
    val supplied = operand.getItemType
    val relation = th.relationship(requiredItemType, supplied)
    if (relation == TypeHierarchy.SAME_TYPE || relation == TypeHierarchy.SUBSUMES) {
      return operand
    } else if (relation == TypeHierarchy.DISJOINT) {
      if (Cardinality.allowsZero(card)) {
      } else if (requiredItemType == AtomicType.STRING && th.isSubType(supplied, AtomicType.ANY_URI)) {
        return operand
      } else {
        val message = role.composeErrorMessage(requiredItemType, operand.getItemType)
        typeError(message, role.getErrorCode)
      }
    }
    this
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod(): Int = {
    var m = Expression.ITERATE_METHOD
    if (!Cardinality.allowsMany(getCardinality)) {
      m |= Expression.EVALUATE_METHOD
    }
    m
  }

  /**
   * Iterate over the sequence of values
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = operand.iterate(context)
    new ItemMappingIterator(base, getMappingFunction(context), true)
  }

  /**
   * Get the mapping function used to implement this item check. This mapping function is applied
   * to each item in the input sequence.
   * @param context The dynamic context used to evaluate the mapping function
   * @return the mapping function. This will be an identity mapping: the output sequence is the same
   * as the input sequence, unless the dynamic type checking reveals an error.
   */
  def getMappingFunction(context: XPathContext): ItemMappingFunction = {
    val map = new ItemCheckMappingFunction()
    map.externalContext = context
    map
  }

  /**
   * Mapping function. This is an identity mapping: either the input items are returned unchanged,
   * or an error is thrown
   */
  private class ItemCheckMappingFunction extends ItemMappingFunction {

    var externalContext: XPathContext = _

    def mapItem(item: Item): Item = {
      testConformance(item, externalContext)
      item
    }
  }

  /**
   * Evaluate as an Item.
   */
  override def evaluateItem(context: XPathContext): Item = {
    val item = operand.evaluateItem(context)
    if (item == null) return null
    testConformance(item, context)
    item
  }

  private def testConformance(_item: Item, context: XPathContext) {
    var item = _item
    if (item.isInstanceOf[AnyURIValue]) {
      item = StringValue.EMPTY_STRING
    }
    if (!requiredItemType.matchesItem(item)) {
      var message: String = null
      message = if (context == null) "Supplied value of type " + Type.displayTypeName(item) + 
        " does not match the required type of " + 
        role.getMessage else role.composeErrorMessage(requiredItemType, SequenceTool.getItemType(item))
      val errorCode = role.getErrorCode
      if ("XPDY0050" == errorCode) {
        dynamicError(message, errorCode)
      } else {
        typeError(message, errorCode)
      }
    }
  }

  /**
   * Determine the data type of the items returned by the expression
   */
  override def getItemType(): ItemType = requiredItemType

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    super.equals(other) &&
      requiredItemType == other.asInstanceOf[ItemChecker].requiredItemType
  }

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = {
    super.hashCode ^ requiredItemType.hashCode
  }
}

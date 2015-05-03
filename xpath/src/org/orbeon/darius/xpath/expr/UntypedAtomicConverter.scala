// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`._
import org.orbeon.darius.xpath.expr.instruct.ForEach
import org.orbeon.darius.xpath.functions.SystemFunction
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.pattern.NodeTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value._

/**
 * An UntypedAtomicConverter is an expression that converts any untypedAtomic items in
 * a sequence to a specified type
 */
class UntypedAtomicConverter(sequence: Expression, 
    var requiredItemType: AtomicType, 
    var allConverted: Boolean, 
    var role: RoleLocator) extends UnaryExpression(sequence) with ItemMappingFunction {

  private var singleton: Boolean = false

  ExpressionTool.copyLocationInfo(sequence, this)

  /**
   * Determine the data type of the items returned by the expression
   *
   */
  override def getItemType: ItemType = {
    val it = operand.getItemType
    singleton = it.isInstanceOf[AtomicType] && !Cardinality.allowsMany(operand.getCardinality)
    if (allConverted) {
      requiredItemType
    } else {
      Type.getCommonSuperType(requiredItemType, operand.getItemType)
    }
  }

  override def computeCardinality(): Int = {
    if (singleton) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      super.computeCardinality()
    }
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (allConverted && requiredItemType == AtomicType.QNAME) {
      typeError("Cannot convert untypedAtomic values to QNames", "XPTY0004")
    }
    operand = visitor.typeCheck(operand, contextItemType)
    if (operand.isInstanceOf[Literal]) {
      return Literal.makeLiteral(SequenceExtent.makeSequenceExtent(iterate(new EarlyEvaluationContext(visitor.getConfiguration))))
    }
    val `type` = operand.getItemType
    if (`type`.isInstanceOf[NodeTest]) {
      return this
    }
    singleton = `type`.isInstanceOf[AtomicType] && !Cardinality.allowsMany(operand.getCardinality)
    operand match {
      case atomizer: Atomizer if `type` == AtomicType.UNTYPED_ATOMIC && requiredItemType == AtomicType.STRING && atomizer.getBaseExpression.getItemType.isInstanceOf[NodeTest] ⇒
        val nodeExp = atomizer.getBaseExpression
        if (nodeExp.getCardinality != StaticProperty.EXACTLY_ONE) {
          val fn = SystemFunction.makeSystemFunction("string", Array(new ContextItemExpression())).asInstanceOf[SystemFunction]
          fn.setContainer(getContainer)
          val map = new ForEach(nodeExp, fn, false)
          map.setContainer(getContainer)
          return map
        } else {
          val fn = SystemFunction.makeSystemFunction("string", Array(nodeExp)).asInstanceOf[SystemFunction]
          fn.setContainer(getContainer)
          return fn
        }
      case _ ⇒
    }
    if (`type` == AtomicType.ANY_ATOMIC || `type`.isInstanceOf[AnyItemType] || 
      `type` == AtomicType.UNTYPED_ATOMIC) {
      return this
    }
    operand
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   * @param visitor         an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[org.orbeon.darius.xpath.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    val e2 = super.optimize(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    operand match {
      case castExpression: CastExpression ⇒
        val it = castExpression.getTargetType
        if (th.isSubType(it, AtomicType.UNTYPED_ATOMIC)) {
          val e = castExpression.getBaseExpression
          val et = e.getItemType
          if (et.isInstanceOf[AtomicType] && th.isSubType(et, requiredItemType)) {
            return e
          }
        }
      case _ ⇒
    }
    this
  }

  /**
   * Determine the special properties of this expression
   *
   * @return [[StaticProperty.NON_CREATIVE]].
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE | StaticProperty.NOT_UNTYPED
  }

  /**
   * Iterate over the sequence of values
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = operand.iterate(context)
    new ItemMappingIterator(base, this, true)
  }

  /**
   * The mapping function that converts untyped atomic values to the required type
   * @return the mapping function
   */
  def mapItem(item: Item): Item = {
    item match {
      case untypedAtomicValue: UntypedAtomicValue ⇒
        convertItem(untypedAtomicValue).asInstanceOf[AtomicValue]
      case _ ⇒
        item
    }
  }

  private def convertItem(item: UntypedAtomicValue): ConversionResult = {
    val `val` = item.convert(requiredItemType)
    `val` match {
      case failure: ValidationFailure ⇒
        var msg = role.composeRequiredMessage(requiredItemType)
        msg += s". ${failure.getMessage}"
        throw new XPathException(msg, role.getErrorCode, this.getSourceLocator)
      case _ ⇒
    }
    `val`
  }

  /**
   * Evaluate as an Item. This should only be called if the UntypedAtomicConverter has cardinality zero-or-one
   */
  override def evaluateItem(context: XPathContext): Item = {
    val item = operand.evaluateItem(context)
    if (item == null) {
      null
    } else item match {
      case untypedAtomicValue: UntypedAtomicValue ⇒
        convertItem(untypedAtomicValue).asAtomic()
      case _ ⇒
        item
    }
  }
}

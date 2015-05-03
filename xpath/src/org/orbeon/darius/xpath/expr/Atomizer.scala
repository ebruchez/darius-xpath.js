// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`._
import org.orbeon.darius.xpath.expr.Atomizer._
import org.orbeon.darius.xpath.expr.instruct.{Block, ValueOf}
import org.orbeon.darius.xpath.om.{Item, NodeInfo, SequenceIterator}
import org.orbeon.darius.xpath.pattern.{EmptySequenceTest, NodeTest}
import org.orbeon.darius.xpath.value.AtomicValue

object Atomizer {

  /**
   * Compute the type that will result from atomizing the result of a given expression
   *
   * @param operand the given expression
   * @param alwaysUntyped true if it is known that nodes will always be untyped
   * @return the item type of the result of evaluating the operand expression, after atomization
   */
  def getAtomizedItemType(operand: Expression, alwaysUntyped: Boolean): ItemType = {
    val in = operand.getItemType
    if (in.isInstanceOf[AnyItemType]) {
      return AtomicType.ANY_ATOMIC
    }
    in match {
      case test: NodeTest ⇒
        if (in.isInstanceOf[EmptySequenceTest]) {
          return in
        }
        val kinds = test.getNodeKindMask
        if (alwaysUntyped) {
          if ((kinds | STRING_KINDS) == STRING_KINDS) {
            return AtomicType.STRING
          }
          if ((kinds | UNTYPED_IF_UNTYPED_KINDS) == UNTYPED_IF_UNTYPED_KINDS) {
            return AtomicType.UNTYPED_ATOMIC
          }
        } else {
          if ((kinds | UNTYPED_KINDS) == UNTYPED_KINDS) {
            return AtomicType.UNTYPED_ATOMIC
          }
        }
        in.getAtomizedItemType
      case _ ⇒
        in
    }
  }

  /**
   * Node kinds whose typed value is always a string
   */
  private val STRING_KINDS = (1 << Type.NAMESPACE) | (1 << Type.COMMENT) | (1 << Type.PROCESSING_INSTRUCTION)

  /**
   * Node kinds whose typed value is always untypedAtomic
   */
  private val UNTYPED_KINDS = (1 << Type.TEXT) | (1 << Type.DOCUMENT)

  /**
   * Node kinds whose typed value is untypedAtomic if the configuration is untyped
   */
  private val UNTYPED_IF_UNTYPED_KINDS = (1 << Type.TEXT) | (1 << Type.ELEMENT) | (1 << Type.DOCUMENT) | 
    (1 << Type.ATTRIBUTE)

  /**
   * Get an iterator that returns the result of atomizing the sequence delivered by the supplied
   * iterator
   * @param base the supplied iterator, the input to atomization
   * @return an iterator that returns atomic values, the result of the atomization
   */
  def getAtomizingIterator(base: SequenceIterator): SequenceIterator = {
    val imf = new ItemMappingFunction() {

      def mapItem(item: Item): Item = return item.getTypedValue
    }
    new ItemMappingIterator(base, imf)
  }
}

/**
 * An Atomizer is an expression corresponding essentially to the fn:data() function: it
 * maps a sequence by replacing nodes with their typed values
 */
class Atomizer(sequence: Expression) extends UnaryExpression(sequence) {

  sequence.setFlattened(true)

  /**
   * Simplify an expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    operand match {
      case literal: Literal ⇒
        val `val` = literal.getValue
        if (`val`.isInstanceOf[AtomicValue]) {
          return operand
        }
        val iter = `val`.iterate()
        while (true) {
          val i = iter.next()
          if (i == null) {
            return operand
          }
          if (i.isInstanceOf[NodeInfo]) {
            return this
          }
        }
      case valueOf: ValueOf ⇒
        return valueOf.convertToCastAsString()
      case _ ⇒
    }
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    val th = TypeHierarchy.getInstance
    visitor.resetStaticProperties()
    val operandType = operand.getItemType
    if (th.isSubType(operandType, AtomicType.ANY_ATOMIC)) {
      return operand
    }
    operand.setFlattened(true)
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
   *                        [[org.orbeon.darius.xpath.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val exp = super.optimize(visitor, contextItemType)
    if (exp == this) {
      val th = TypeHierarchy.getInstance
      if (th.isSubType(operand.getItemType, AtomicType.ANY_ATOMIC)) {
        return operand
      }
      operand match {
        case valueOf: ValueOf ⇒
          return valueOf.convertToCastAsString()
        case _ ⇒
      }
      operand match {
        case block: Block ⇒
          val children = block.getChildren
          val atomizedChildren = new Array[Expression](children.length)
          for (i ← 0 until children.length) {
            atomizedChildren(i) = new Atomizer(children(i))
          }
          val newBlock = new Block()
          newBlock.setChildren(atomizedChildren)
          return newBlock.typeCheck(visitor, contextItemType).optimize(visitor, contextItemType)
        case _ ⇒
      }
    }
    exp
  }

  /**
   * Determine the special properties of this expression
   * @return [[StaticProperty.NON_CREATIVE]].
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Iterate over the sequence of values
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = operand.iterate(context)
    getAtomizingIterator(base)
  }

  /**
   * Evaluate as an Item. This should only be called if the Atomizer has cardinality zero-or-one,
   * which will only be the case if the underlying expression has cardinality zero-or-one.
   */
  override def evaluateItem(context: XPathContext): Item = {
    val i = operand.evaluateItem(context)
    if (i == null) null else i.getTypedValue
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER. For this class, the
   * result is always an atomic type, but it might be more specific.
   */
  override def getItemType: ItemType = getAtomizedItemType(operand, alwaysUntyped = true)

  /**
   * Determine the static cardinality of the expression
   */
  override def computeCardinality(): Int = operand.getCardinality
}

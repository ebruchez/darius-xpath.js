// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.functions.Remove._
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.{AtomicValue, DecimalValue, IntegerValue, NumericValue}

object Remove {

  /**
   * Mapping function to return the item unchanged except for the item at the specified position
   */
  class RemoveMappingFunction(var removeIndex: Int) extends ItemMappingFunction with StatefulMappingFunction {

    private var position: Int = 1

    /**
     * Map one item to another item.
     *
     * @param item The input item to be mapped.
     * @return either the output item, or null.
     */
    def mapItem(item: Item): Item = {
      val result = if (position == removeIndex) null else item
      position += 1
      result
    }

    /**
     * Return a clone of this MappingFunction, with the state reset to its state at the beginning
     * of the underlying iteration
     *
     * @return a clone of this MappingFunction
     * @param newBaseIterator
     */
    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = new RemoveMappingFunction(removeIndex)
  }
}

/**
 * The XPath 2.0 remove() function
 */
class Remove extends SystemFunction {

  def newInstance(): Remove = new Remove()

  /**
   * Simplify. Recognize remove(seq, 1) as a TailExpression.
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    val exp = super.simplify(visitor)
    exp match {
      case remove: Remove ⇒
        remove.simplifyAsTailExpression()
      case _ ⇒
        exp
    }
  }

  /**
   * Simplify. Recognize remove(seq, 1) as a TailExpression. This
   * is worth doing because tail expressions used in a recursive call
   * are handled specially.
   */
  private def simplifyAsTailExpression(): Expression = {
    if (Literal.isAtomic(argument(1))) {
      try {
        val value = argument(1).asInstanceOf[Literal].getValue.asInstanceOf[IntegerValue]
          .intValue()
        if (value <= 0) {
          return argument(0)
        } else if (value == 1) {
          val t = SystemFunction.makeSystemFunction("subsequence", Array(argument(0), new Literal(DecimalValue.TWO)))
          ExpressionTool.copyLocationInfo(this, t)
          return t
        }
      } catch {
        case err: XPathException ⇒ return this
      }
    }
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
    val e = super.optimize(visitor, contextItemType)
    if (e == this) {
      return simplifyAsTailExpression()
    }
    e
  }

  /**
   * Determine the data type of the items in the sequence
   * @return the type of the input sequence
   */
  override def getItemType: ItemType = argument(0).getItemType

  /**
   * Evaluate the function to return an iteration of selected nodes.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val seq = argument(0).iterate(context)
    val n0 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    val n = n0.asInstanceOf[NumericValue]
    val pos = n.intValue().toInt
    if (pos < 1) {
      return seq
    }
    val function = new RemoveMappingFunction(pos)
    new ItemMappingIterator(seq, function)
  }
}

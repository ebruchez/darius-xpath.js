package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.NumericValue
import client.net.sf.saxon.ce.value.IntegerValue
import Remove._
//remove if not needed
import scala.collection.JavaConversions._

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
      (if (position += 1 == removeIndex) null else item)
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
  def simplify(visitor: ExpressionVisitor): Expression = {
    val exp = super.simplify(visitor)
    if (exp.isInstanceOf[Remove]) {
      exp.asInstanceOf[Remove].simplifyAsTailExpression()
    } else {
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
          val t = SystemFunction.makeSystemFunction("subsequence", Array(argument(0), new Literal(IntegerValue.TWO)))
          ExpressionTool.copyLocationInfo(this, t)
          return t
        }
      } catch {
        case err: XPathException => return this
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
   *                        {@link client.net.sf.saxon.ce.type.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
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
  def getItemType(): ItemType = argument(0).getItemType

  /**
   * Evaluate the function to return an iteration of selected nodes.
   */
  def iterate(context: XPathContext): SequenceIterator = {
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

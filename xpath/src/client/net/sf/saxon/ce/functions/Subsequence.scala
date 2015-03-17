// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.Subsequence._
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.value.{DoubleValue, IntegerValue, NumericValue}

object Subsequence {

  /**
   * Static factory method. Creates a SubsequenceIterator, unless for example the base Iterator is an
   * ArrayIterator, in which case it optimizes by creating a new ArrayIterator directly over the
   * underlying array. This optimization is important when doing recursion over a node-set using
   * repeated calls of $nodes[position()>1]
   * @param base   An iteration of the items to be filtered
   * @param min    The position of the first item to be included (base 1)
   * @param max    The position of the last item to be included (base 1).  May be Integer.MAX_VALUE
   * @return an iterator over the requested subsequence
   * @throws XPathException (probably can't happen)
   */
  def makeIterator(base: SequenceIterator, min: Int, max: Int): SequenceIterator = {
    new ItemMappingIterator(base, new SubsequenceMappingFunction(base, min, max))
  }

  private class SubsequenceMappingFunction(var base: SequenceIterator, var min: Int, var max: Int)
      extends ItemMappingFunction with StatefulMappingFunction {

    private var pos: Int = 0

    /**
     * Map one item to another item.
     *
     * @param item The input item to be mapped.
     * @return either the output item, or null.
     */
    def mapItem(item: Item): Item = {
      val position = pos
      if (position > max) {
        //println("xxxx")
        throw new ItemMappingIterator.EarlyExitException()
      }
      if (position >= min) item else null
    }

    /**
     * Return a clone of this MappingFunction, with the state reset to its state at the beginning
     * of the underlying iteration
     *
     * @return a clone of this MappingFunction
     * @param newBaseIterator the cloned SequenceIterator to which the new mapping function will be applied
     */
    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = {
      new SubsequenceMappingFunction(newBaseIterator, min, max)
    }
  }
}

/**
 * Implements the XPath 2.0 subsequence()  function
 */
class Subsequence extends SystemFunction {

  def newInstance(): Subsequence = new Subsequence()

  /**
   * Determine the data type of the items in the sequence
   * @return the type of the argument
   */
  override def getItemType(): ItemType = argument(0).getItemType

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-significant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = argument(0).getSpecialProperties

  /**
   * Determine the cardinality of the function.
   */
  override def computeCardinality(): Int = {
    if (getNumberOfArguments == 3 && Literal.isConstantOne(argument(2))) {
      return StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    argument(0).getCardinality | StaticProperty.ALLOWS_ZERO
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
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    this
  }

  /**
   * Evaluate the function to return an iteration of selected nodes.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val seq = argument(0).iterate(context)
    var startVal = argument(1).evaluateItem(context).asInstanceOf[DoubleValue]
    if (startVal.isNaN) {
      return EmptyIterator.getInstance
    }
    if (startVal.compareTo(IntegerValue.MAX_LONG) > 0) {
      return EmptyIterator.getInstance
    }
    startVal = startVal.round().asInstanceOf[DoubleValue]
    var lstart: Int = 0
    lstart = if (startVal.compareTo(IntegerValue.PLUS_ONE) <= 0) 1 else startVal.intValue()
    var lend: Int = 0
    if (argument.length == 2) {
      lend = Integer.MAX_VALUE
    } else {
      var lengthVal = argument(2).evaluateItem(context).asInstanceOf[DoubleValue]
      if (lengthVal.isNaN) {
        return EmptyIterator.getInstance
      }
      lengthVal = lengthVal.round().asInstanceOf[DoubleValue]
      if (lengthVal.compareTo(IntegerValue.ZERO) <= 0) {
        return EmptyIterator.getInstance
      }
      var rend = ArithmeticExpression.compute(startVal, Token.PLUS, lengthVal, context).asInstanceOf[NumericValue]
      if (rend.isNaN) {
        return EmptyIterator.getInstance
      }
      rend = ArithmeticExpression.compute(rend, Token.MINUS, IntegerValue.PLUS_ONE, context).asInstanceOf[NumericValue]
      if (rend.compareTo(IntegerValue.ZERO) <= 0) {
        return EmptyIterator.getInstance
      }
      lend = rend.intValue()
    }
    makeIterator(seq, lstart, lend)
  }
}

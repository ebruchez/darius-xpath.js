// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`.{ItemType, Type, TypeHierarchy}
import org.orbeon.darius.xpath.expr.AdjacentTextNodeMerger._
import org.orbeon.darius.xpath.expr.instruct.{Block, Choose, ValueOf}
import org.orbeon.darius.xpath.om.{Item, NodeInfo, SequenceIterator}
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.tree.util.{FastStringBuffer, Orphan}
import org.orbeon.darius.xpath.value.Cardinality

import scala.util.control.Breaks

object AdjacentTextNodeMerger {

  /**
   * AdjacentTextNodeMergingIterator is an iterator that eliminates zero-length text nodes
   * and merges adjacent text nodes from the underlying iterator
   */
  class AdjacentTextNodeMergingIterator(var base: SequenceIterator) extends SequenceIterator {

    private var current: Item = _

    private var _next: Item = base.next()

    def next(): Item = {
      current = next
      if (current == null) {
        return null
      }
      _next = base.next()
      if (isTextNode(current)) {
        val fsb = new FastStringBuffer(FastStringBuffer.MEDIUM)
        fsb.append(current.getStringValue)
        while (next != null && isTextNode(next)) {
          fsb.append(next.getStringValue)
          _next = base.next()
        }
        if (fsb.length == 0) {
          next()
        } else {
          val o = new Orphan()
          o.setNodeKind(Type.TEXT)
          o.setStringValue(fsb)
          current = o
          current
        }
      } else {
        current
      }
    }

    def getAnother: SequenceIterator = {
      new AdjacentTextNodeMergingIterator(base.getAnother)
    }
  }

  /**
   * Ask whether an item is a text node
   * @param item the item in question
   * @return true if the item is a node of kind text
   */
  def isTextNode(item: Item): Boolean = {
    item.isInstanceOf[NodeInfo] && 
      item.asInstanceOf[NodeInfo].getNodeKind == Type.TEXT
  }
}

/**
 * This class performs the first phase of processing in "constructing simple content":
 * it takes an input sequence, eliminates empty text nodes, and combines adjacent text nodes
 * into one.
 * @since 9.3
 */
class AdjacentTextNodeMerger(p0: Expression) extends UnaryExpression(p0) {

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.typeCheck(visitor, contextItemType)
    if (e != this) {
      return e
    }
    val th = TypeHierarchy.getInstance
    if (th.relationship(getBaseExpression.getItemType, NodeKindTest.TEXT) == 
      TypeHierarchy.DISJOINT) {
      return getBaseExpression
    }
    if (!Cardinality.allowsMany(getBaseExpression.getCardinality)) {
      return getBaseExpression
    }
    getBaseExpression match {
      case choose: Choose ⇒
        val actions = choose.getActions
        for (i ← 0 until actions.length) {
          val atm2 = new AdjacentTextNodeMerger(actions(i))
          actions(i) = atm2.typeCheck(visitor, contextItemType)
        }
        return choose
      case _ ⇒
    }
    getBaseExpression match {
      case block: Block ⇒
        val actions = block.getChildren
        var prevtext = false
        var needed = false
        var maybeEmpty = false
        import Breaks._
        breakable {
          for (i ← 0 until actions.length) {
            var maybetext: Boolean = false
            actions(i) match {
              case of: ValueOf ⇒
                maybetext = true
                val content = of.getContentExpression
                content match {
                  case literal: StringLiteral ⇒
                    maybeEmpty |= literal.getStringValue.length ==
                      0
                  case _ ⇒
                    maybeEmpty = true
                }
              case _ ⇒
                maybetext = th.relationship(actions(i).getItemType, NodeKindTest.TEXT) !=
                  TypeHierarchy.DISJOINT
                maybeEmpty |= maybetext
            }
            if (prevtext && maybetext) {
              needed = true
              break()
            }
            if (maybetext && Cardinality.allowsMany(actions(i).getCardinality)) {
              needed = true
              break()
            }
            prevtext = maybetext
          }
        }
        if (!needed) {
          if (maybeEmpty) {
            return new EmptyTextNodeRemover(block)
          } else {
            return block
          }
        }
      case _ ⇒
    }
    this
  }

  /**
   * Determine the data type of the expression, if possible. The default
   * implementation for unary expressions returns the item type of the operand
   * @return the item type of the items in the result sequence, insofar as this
   *         is known statically.
   */
  override def getItemType: ItemType = getBaseExpression.getItemType

  override def computeCardinality(): Int = {
    getBaseExpression.getCardinality | StaticProperty.ALLOWS_ZERO
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered.
   */
  override def getImplementationMethod: Int = {
    Expression.PROCESS_METHOD | Expression.ITERATE_METHOD
  }

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation handles iteration for expressions that
   * return singleton values: for non-singleton expressions, the subclass must
   * provide its own implementation.
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *         of the expression
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if any dynamic error occurs evaluating the
   *          expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    new AdjacentTextNodeMergingIterator(getBaseExpression.iterate(context))
  }
}

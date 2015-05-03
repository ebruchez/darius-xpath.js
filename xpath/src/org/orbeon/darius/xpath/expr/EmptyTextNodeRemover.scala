// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`.{ItemType, Type}
import org.orbeon.darius.xpath.om.{Item, NodeInfo, SequenceIterator}

/**
 * This class performs the first phase of processing in "constructing simple content":
 * it takes an input sequence, eliminates empty text nodes, and combines adjacent text nodes
 * into one.
 * @since 9.3
 */
class EmptyTextNodeRemover(p0: Expression) extends UnaryExpression(p0) {

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
  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

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
    val map = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        item match {
          case info: NodeInfo if info.getNodeKind == Type.TEXT && item.getStringValue.length == 0 ⇒
            return null
          case _ ⇒
            return item
        }
      }
    }
    new ItemMappingIterator(getBaseExpression.iterate(context), map)
  }
}

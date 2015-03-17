// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.value.AtomicValue

/**
 * A NumericPromoter performs numeric promotion on each item in a supplied sequence.
 * There are two subclasses, to handle promotion to double and promotion to float
 */
abstract class NumericPromoter(exp: Expression) extends UnaryExpression(exp) {

  /**
   * Simplify an expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    this
  }

  /**
   * Iterate over the sequence of values
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = operand.iterate(context)
    val promoter = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        return promote(item.asInstanceOf[AtomicValue])
      }
    }
    new ItemMappingIterator(base, promoter, true)
  }

  /**
   * Evaluate as an Item. This should only be called if the expression has cardinality zero-or-one
   */
  override def evaluateItem(context: XPathContext): Item = {
    val item = operand.evaluateItem(context)
    if (item == null) return null
    promote(item.asInstanceOf[AtomicValue])
  }

  /**
   * Perform the promotion
   * @param value the numeric or untyped atomic value to be promoted
   * @return the value that results from the promotion
   */
  protected def promote(value: AtomicValue): AtomicValue
}

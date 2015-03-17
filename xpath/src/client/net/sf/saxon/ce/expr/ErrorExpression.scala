// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AnyItemType, ItemType}
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.trans.XPathException

import scala.beans.BeanProperty

/**
 * Error expression: this expression is generated when the supplied expression cannot be
 * parsed, and the containing element enables forwards-compatible processing. It defers
 * the generation of an error message until an attempt is made to evaluate the expression
 */
class ErrorExpression(@BeanProperty var exception: XPathException) extends Expression {

  exception.setLocator(getSourceLocator)

  /**
   * Type-check the expression.
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  /**
   * Evaluate the expression. This always throws the exception registered when the expression
   * was first parsed.
   */
  override def evaluateItem(context: XPathContext): Item = {
    val err = new XPathException(exception.getMessage)
    err.setLocator(getSourceLocator)
    err.setErrorCodeQName(exception.getErrorCodeQName)
    throw err
  }

  /**
   * Iterate over the expression. This always throws the exception registered when the expression
   * was first parsed.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    evaluateItem(context)
    null
  }

  /**
   * Determine the data type of the expression, if possible
   * @return Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = AnyItemType.getInstance

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  private def copy(): Expression = new ErrorExpression(exception)
}

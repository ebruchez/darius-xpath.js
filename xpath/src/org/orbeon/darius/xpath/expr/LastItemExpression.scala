// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.tree.iter.GroundedIterator

/**
 * A LastItemExpression returns the last item in the sequence returned by a given
 * base expression. The evaluation strategy is to read the input sequence with a one-item lookahead.
 */
class LastItemExpression(base: Expression) extends SingleItemFilter {

  operand = base

  adoptChildExpression(base)

  computeStaticProperties()

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val forwards = operand.iterate(context)
    forwards match {
      case iterator: GroundedIterator ⇒
        val `val` = iterator.materialize()
        if (`val` != null) {
          return `val`.itemAt(`val`.getLength - 1)
        }
      case _ ⇒
    }
    var current: Item = null
    while (true) {
      val item = forwards.next()
      if (item == null) {
        return current
      }
      current = item
    }
    throw new IllegalStateException
  }
}

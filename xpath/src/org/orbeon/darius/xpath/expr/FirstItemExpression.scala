// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.om.Item

/**
 * A FirstItemExpression returns the first item in the sequence returned by a given
 * base expression
 */
class FirstItemExpression(base: Expression) extends SingleItemFilter {

  operand = base

  adoptChildExpression(base)

  computeStaticProperties()

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val iter = operand.iterate(context)
    iter.next()
  }
}

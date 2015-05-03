// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.{Expression, XPathContext}
import org.orbeon.darius.xpath.functions.Exists._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value.BooleanValue

object Exists {
  val EXISTS = 2
  val EMPTY = 3
}

/**
 * Implementation of the fn:exists and fn:empty functions
 */
class Exists(_operation: Int) extends Aggregate {

  this.operation = _operation

  def newInstance(): Exists = new Exists(operation)

  /**
   * Return the negation of the expression
   * @return the negation of the expression
   */
  def negate(): Expression = {
    val fc = SystemFunction.makeSystemFunction(if (operation == EXISTS) "empty" else "exists", getArguments)
    fc.setSourceLocator(getSourceLocator)
    fc
  }

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Evaluate the function in a boolean context
   */
  override def effectiveBooleanValue(c: XPathContext): Boolean = {
    val next = argument(0).iterate(c).next()
    if (operation == EXISTS) next != null else next == null
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.AtomicValue

object Component {

  val YEAR = 1
  val MONTH = 2
  val DAY = 3
  val HOURS = 4
  val MINUTES = 5
  val SECONDS = 6
  val TIMEZONE = 7
  val LOCALNAME = 8
  val NAMESPACE = 9
  val PREFIX = 10
  val MICROSECONDS = 11
  val WHOLE_SECONDS = 12
  val YEAR_ALLOWING_ZERO = 13
}

/**
 * This class supports the get_X_from_Y functions defined in XPath 2.0
 */
class Component(_operation: Int) extends SystemFunction {

  this.operation = _operation

  def newInstance(): Component = new Component(operation)

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val arg = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg == null) {
      return null
    }
    arg.getComponent(operation)
  }
}

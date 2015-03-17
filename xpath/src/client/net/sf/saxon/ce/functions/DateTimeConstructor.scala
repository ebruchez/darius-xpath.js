// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{AtomicValue, DateTimeValue, DateValue, TimeValue}

/**
 * This class supports the dateTime($date, $time) function
 */
class DateTimeConstructor extends SystemFunction {

  def newInstance(): DateTimeConstructor = new DateTimeConstructor()

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    val arg1 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    try {
      DateTimeValue.makeDateTimeValue(arg0.asInstanceOf[DateValue], arg1.asInstanceOf[TimeValue])
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
  }
}

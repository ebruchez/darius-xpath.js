package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.DateTimeValue
import client.net.sf.saxon.ce.value.DateValue
import client.net.sf.saxon.ce.value.TimeValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the dateTime($date, $time) function
 */
class DateTimeConstructor extends SystemFunction {

  def newInstance(): DateTimeConstructor = new DateTimeConstructor()

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
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

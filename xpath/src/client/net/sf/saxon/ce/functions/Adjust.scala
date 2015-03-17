package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.CalendarValue
import client.net.sf.saxon.ce.value.DayTimeDurationValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class implements the XPath 2.0 functions
 * adjust-date-to-timezone(), adjust-time-timezone(), and adjust-dateTime-timezone().
 */
class Adjust extends SystemFunction {

  def newInstance(): Adjust = new Adjust()

  /**
   * Evaluate in a general context
   */
  def evaluateItem(context: XPathContext): Item = {
    val av1 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (av1 == null) {
      return null
    }
    val in = av1.asInstanceOf[CalendarValue]
    val nargs = argument.length
    var tz: DayTimeDurationValue = null
    if (nargs == 1) {
      in.adjustTimezone(context.getImplicitTimezone)
    } else {
      val av2 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
      if (av2 == null) {
        return in.removeTimezone()
      }
      tz = av2.asInstanceOf[DayTimeDurationValue]
      val microseconds = tz.getLengthInMicroseconds
      if (microseconds % 60000000 != 0) {
        dynamicError("Timezone is not an integral number of minutes", "FODT0003")
      }
      val tzminutes = (microseconds / 60000000).toInt
      if (Math.abs(tzminutes) > 14 * 60) {
        dynamicError("Timezone out of range (-14:00 to +14:00)", "FODT0003")
      }
      in.adjustTimezone(tzminutes)
    }
  }
}

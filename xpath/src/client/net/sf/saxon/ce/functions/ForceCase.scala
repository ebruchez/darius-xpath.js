package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.StringValue
import ForceCase._
//remove if not needed
import scala.collection.JavaConversions._

object ForceCase {

  val UPPERCASE = 0

  val LOWERCASE = 1
}

/**
 * This class implements the upper-case() and lower-case() functions
 */
class ForceCase(operation: Int) extends SystemFunction {

  this.operation = operation

  def newInstance(): ForceCase = new ForceCase(operation)

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    val sv = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    if (sv == null) {
      return StringValue.EMPTY_STRING
    }
    var s = sv.getStringValue
    s = if (operation == UPPERCASE) s.toUpperCase() else s.toLowerCase()
    StringValue.makeStringValue(s)
  }
}

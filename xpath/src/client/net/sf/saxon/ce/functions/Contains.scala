package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.CodepointCollator
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value._
import Contains._
//remove if not needed
import scala.collection.JavaConversions._

object Contains {

  val CONTAINS = 0

  val STARTS_WITH = 1

  val ENDS_WITH = 2
}

/**
 * Implements the fn:contains() function, also starts-with() and ends-with()
 */
class Contains(op: Int) extends CollatingFunction {

  operation = op

  def newInstance(): Contains = new Contains(operation)

  def evaluateItem(context: XPathContext): Item = {
    var result: Boolean = false
    val arg1 = argument(1).evaluateItem(context).asInstanceOf[StringValue]
    if (arg1 == null || arg1.isZeroLength) {
      result = true
    } else {
      val arg0 = argument(0).evaluateItem(context).asInstanceOf[StringValue]
      if (arg0 == null || arg0.isZeroLength) {
        result = false
      } else {
        val s0 = arg0.getStringValue
        val s1 = arg1.getStringValue
        val collator = getCollator(2, context)
        if (collator.isInstanceOf[CodepointCollator]) operation match {
          case CONTAINS => result = s0.indexOf(s1, 0) >= 0
          case STARTS_WITH => result = s0.startsWith(s1, 0)
          case ENDS_WITH |  => result = s0.endsWith(s1)
        } else {
          doesNotSupportSubstringMatching(context)
          result = false
        }
      }
    }
    BooleanValue.get(result)
  }
}

package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.CodepointCollator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.StringValue
import SubstringAfterBefore._
//remove if not needed
import scala.collection.JavaConversions._

object SubstringAfterBefore {

  val AFTER = 1

  val BEFORE = 2
}

/**
 * Implements the fn:substring-after() function
 */
class SubstringAfterBefore(operation: Int) extends CollatingFunction {

  this.operation = operation

  def newInstance(): SubstringAfterBefore = new SubstringAfterBefore(operation)

  /**
   * Evaluate the function
   */
  def evaluateItem(context: XPathContext): Item = {
    if (!(stringCollator.isInstanceOf[CodepointCollator])) {
      doesNotSupportSubstringMatching(context)
    }
    var result: String = null
    var arg1 = argument(0).evaluateItem(context).asInstanceOf[StringValue]
    var arg2 = argument(1).evaluateItem(context).asInstanceOf[StringValue]
    if (arg1 == null) {
      arg1 = StringValue.EMPTY_STRING
    }
    if (arg2 == null) {
      arg2 = StringValue.EMPTY_STRING
    }
    val s1 = arg1.getStringValue
    val s2 = arg2.getStringValue
    val index = s1.indexOf(s2)
    if (index < 0) {
      return StringValue.EMPTY_STRING
    } else {
      if (operation == AFTER) {
        if (s2.isEmpty) {
          return arg1
        }
        if (s1.isEmpty) {
          return StringValue.EMPTY_STRING
        }
        result = s1.substring(index + s2.length)
      } else {
        if (s1.isEmpty || s2.isEmpty) {
          return StringValue.EMPTY_STRING
        }
        result = s1.substring(0, index)
      }
    }
    val s = StringValue.makeStringValue(result)
    if (arg1.isKnownToContainNoSurrogates) {
      s.setContainsNoSurrogates()
    }
    s
  }
}

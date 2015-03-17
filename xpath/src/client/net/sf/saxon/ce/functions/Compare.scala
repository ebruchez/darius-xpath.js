package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.GenericAtomicComparer
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.IntegerValue
//remove if not needed
import scala.collection.JavaConversions._

class Compare extends CollatingFunction {

  override def newInstance(): Compare = new Compare()

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg0 == null) {
      return null
    }
    val arg1 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg1 == null) {
      return null
    }
    val collator = getAtomicComparer(2, context)
    val result = collator.compareAtomicValues(arg0, arg1)
    if (result < 0) {
      IntegerValue.MINUS_ONE
    } else if (result > 0) {
      IntegerValue.PLUS_ONE
    } else {
      IntegerValue.ZERO
    }
  }
}

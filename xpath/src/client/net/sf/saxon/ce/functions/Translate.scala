package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implement the XPath translate() function
 */
class Translate extends SystemFunction {

  def newInstance(): Translate = new Translate()

  /**
   * Evaluate the function
   */
  def evaluateItem(context: XPathContext): Item = {
    val sv1 = argument(0).evaluateItem(context).asInstanceOf[StringValue]
    if (sv1 == null) {
      return StringValue.EMPTY_STRING
    }
    val sv2 = argument(1).evaluateItem(context).asInstanceOf[StringValue]
    val sv3 = argument(2).evaluateItem(context).asInstanceOf[StringValue]
    val a1 = sv1.expand()
    val a2 = sv2.expand()
    val a3 = sv3.expand()
    val length1 = a1.length
    val length2 = a2.length
    val sb = new FastStringBuffer(length1)
    inputLoop: for (i <- 0 until length1) {
      val ch = a1(i)
      for (j <- 0 until length2 if a2(j) == ch) {
        if (j < a3.length) {
          sb.appendWideChar(a3(j))
        } else {
        }
        //continue
      }
      sb.appendWideChar(ch)
    }
    StringValue.makeStringValue(sb)
  }
}

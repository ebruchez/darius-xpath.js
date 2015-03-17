package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AnyURIValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the base-uri() function in XPath 2.0
 */
class BaseURI extends SystemFunction {

  def newInstance(): BaseURI = new BaseURI()

  /**
   * Evaluate the function at run-time
   */
  def evaluateItem(c: XPathContext): Item = {
    val node = argument(0).evaluateItem(c).asInstanceOf[NodeInfo]
    if (node == null) {
      return null
    }
    val s = node.getBaseURI
    if (s == null) {
      return null
    }
    new AnyURIValue(s)
  }
}

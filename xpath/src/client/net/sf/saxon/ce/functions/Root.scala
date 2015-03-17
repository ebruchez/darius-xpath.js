package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implement the XPath 2.0 root() function
 */
class Root extends SystemFunction {

  def newInstance(): Root = new Root()

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-significant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  def computeSpecialProperties(): Int = {
    var prop = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.NON_CREATIVE
    if ((getNumberOfArguments == 0) || 
      (argument(0).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    val start = argument(0).evaluateItem(c).asInstanceOf[NodeInfo]
    if (start == null) {
      return null
    }
    start.getRoot
  }
}

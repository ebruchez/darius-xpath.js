package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.InscopeNamespaceResolver
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NamespaceResolver
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AnyURIValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the function namespace-uri-for-prefix()
 */
class NamespaceForPrefix extends SystemFunction {

  def newInstance(): NamespaceForPrefix = new NamespaceForPrefix()

  /**
   * Evaluate the function
   * @param context the XPath dynamic context
   * @return the URI corresponding to the prefix supplied in the first argument, or null
   * if the prefix is not in scope
   * @throws XPathException if a failure occurs evaluating the arguments
   */
  def evaluateItem(context: XPathContext): Item = {
    val element = argument(1).evaluateItem(context).asInstanceOf[NodeInfo]
    val p = argument(0).evaluateItem(context).asInstanceOf[StringValue]
    var prefix: String = null
    prefix = if (p == null) "" else p.getStringValue
    val resolver = new InscopeNamespaceResolver(element)
    val uri = resolver.getURIForPrefix(prefix, true)
    if (uri == null) {
      return null
    }
    new AnyURIValue(uri)
  }
}

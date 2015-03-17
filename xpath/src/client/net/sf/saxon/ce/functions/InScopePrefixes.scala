package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.value.StringValue
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.ListIterator
import client.net.sf.saxon.ce.tree.util.NamespaceIterator
import java.util.ArrayList
import java.util.Iterator
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the function in-scope-prefixes()
 */
class InScopePrefixes extends SystemFunction {

  def newInstance(): InScopePrefixes = new InScopePrefixes()

  /**
   * Iterator over the results of the expression
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val element = argument(0).evaluateItem(context).asInstanceOf[NodeInfo]
    val iter = NamespaceIterator.iterateNamespaces(element)
    val prefixes = new ArrayList[StringValue]()
    prefixes.add(new StringValue("xml"))
    while (iter.hasNext) {
      prefixes.add(new StringValue(iter.next().getPrefix))
    }
    new ListIterator(prefixes)
  }
}

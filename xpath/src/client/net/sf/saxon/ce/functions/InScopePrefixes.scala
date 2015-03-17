// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.{NodeInfo, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.ArrayList
import client.net.sf.saxon.ce.tree.iter.ListIterator
import client.net.sf.saxon.ce.tree.util.NamespaceIterator
import client.net.sf.saxon.ce.value.StringValue

/**
 * This class supports the function in-scope-prefixes()
 */
class InScopePrefixes extends SystemFunction {

  def newInstance(): InScopePrefixes = new InScopePrefixes()

  /**
   * Iterator over the results of the expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
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

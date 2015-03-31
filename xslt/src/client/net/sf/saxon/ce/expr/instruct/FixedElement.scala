// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction that creates an element node whose name is known statically.
 * Used for literal results elements in XSLT, for direct element constructors
 * in XQuery, and for xsl:element in cases where the name and namespace are
 * known statically.
 */
class FixedElement(var nameCode: StructuredQName, protected var namespaceCodes: Array[NamespaceBinding], inheritNamespaces: Boolean)
    extends ElementCreator {

  this.inheritNamespaces = inheritNamespaces

  /**
   * Callback from the superclass ElementCreator to get the nameCode
   * for the element name
   *
   * @param context The evaluation context (not used)
   * @param copiedNode
   * @return the name code for the element name
   */
  def getNameCode(context: XPathContext, copiedNode: NodeInfo): StructuredQName = nameCode

  def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String = getBaseURI

  /**
   * Callback from the superclass ElementCreator to output the namespace nodes
   * @param context The evaluation context (not used)
   * @param out The receiver to handle the output
   * @param nameCode
   * @param copiedNode
   */
  protected def outputNamespaceNodes(context: XPathContext, 
      out: Receiver, 
      nameCode: StructuredQName, 
      copiedNode: NodeInfo): Unit = {
    if (namespaceCodes != null) {
      for (namespaceCode ← namespaceCodes) {
        out.namespace(namespaceCode, 0)
      }
    }
  }
}

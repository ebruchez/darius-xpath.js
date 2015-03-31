// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ProcInstImpl is an implementation of ProcInstInfo used by the Propagator to construct
 * its trees.
 * @author Michael H. Kay
 */
class ProcInstImpl(var localName: String, var content: String) extends NodeImpl {

  /**
   * Get the name of the node
   *
   * @return the name of the node, as a StructuredQName. Return null for an unnamed node.
   */
  override def getNodeName(): StructuredQName = new StructuredQName("", "", localName)

  def getStringValue(): String = content

  /**
   * Get the typed value of this node.
   * Returns the string value, as an instance of xs:string
   */
  def getTypedValue(): AtomicValue = new StringValue(getStringValue)

  def getNodeKind(): Int = Type.PROCESSING_INSTRUCTION

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int): Unit = {
    out.processingInstruction(getLocalPart, content)
  }
}

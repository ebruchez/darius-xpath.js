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
  def copy(out: Receiver, copyOptions: Int) {
    out.processingInstruction(getLocalPart, content)
  }
}

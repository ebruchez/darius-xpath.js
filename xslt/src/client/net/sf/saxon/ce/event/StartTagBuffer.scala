// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.om.AttributeCollection
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import java.util.ArrayList
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * StartTagBuffer is a ProxyReceiver that buffers attributes and namespace events within a start tag.
 * It maintains details of the namespace context, and a full set of attribute information, on behalf
 * of other filters that need access to namespace information or need to process attributes in arbitrary
 * order.
 */
class StartTagBuffer extends ProxyReceiver {

  protected var elementNameCode: StructuredQName = _

  protected var elementProperties: Int = _

  protected var bufferedAttributes: AttributeCollection = _

  protected var bufferedNamespaces: List[NamespaceBinding] = new ArrayList[NamespaceBinding]()

  var attCount: Int = 0

  /**
   * Set the pipeline configuration
   * @param pipe the pipeline configuration
   */
  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    super.setPipelineConfiguration(pipe)
    bufferedAttributes = new AttributeCollection()
  }

  /**
   * startElement
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    elementNameCode = qName
    elementProperties = properties
    bufferedAttributes.clear()
    bufferedNamespaces.clear()
    attCount = 0
  }

  override def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    bufferedNamespaces.add(nsBinding)
  }

  /**
   * Notify an attribute. Attributes are notified after the startElement event, and before any
   * children. Namespaces and attributes may be intermingled.
   *
   *
   * @param nameCode   The name of the attribute, as held in the name pool
   * @throws IllegalStateException: attempt to output an attribute when there is no open element
   *                                start tag
   */
  def attribute(nameCode: StructuredQName, value: CharSequence): Unit = {
    bufferedAttributes.addAttribute(nameCode, value.toString)
  }

  /**
   * startContent: Add any namespace undeclarations needed to stop
   * namespaces being inherited from parent elements
   */
  def startContent(): Unit = {
    nextReceiver.startElement(elementNameCode, elementProperties)
    val length = bufferedAttributes.getLength
    for (i <- 0 until length) {
      nextReceiver.attribute(bufferedAttributes.getStructuredQName(i), bufferedAttributes.getValue(i))
    }
    for (nb <- bufferedNamespaces) {
      nextReceiver.namespace(nb, 0)
    }
    nextReceiver.startContent()
  }

  /**
   * Get the value of the current attribute with a given nameCode
   * @param uri the namespace of the required attribute
   * @param local the local name of the required attribute
   * @return the attribute value, or null if the attribute is not present
   */
  def getAttribute(uri: String, local: String): String = bufferedAttributes.getValue(uri, local)
}

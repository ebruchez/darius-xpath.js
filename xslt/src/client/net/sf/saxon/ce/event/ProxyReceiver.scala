// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.event

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.NamespaceBinding
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A ProxyReceiver is an Receiver that filters data before passing it to another
 * underlying Receiver.
 */
abstract class ProxyReceiver extends SequenceReceiver {

  protected var nextReceiver: Receiver = _

  def setSystemId(systemId: String): Unit = {
    if (systemId != this.systemId) {
      this.systemId = systemId
      if (nextReceiver != null) {
        nextReceiver.setSystemId(systemId)
      }
    }
  }

  /**
   * Set the underlying receiver. This call is mandatory before using the Receiver.
   * @param receiver the underlying receiver, the one that is to receive events after processing
   * by this filter.
   */
  def setUnderlyingReceiver(receiver: Receiver): Unit = {
    if (receiver != nextReceiver) {
      nextReceiver = receiver
      if (pipelineConfiguration != null && receiver != null) {
        nextReceiver.setPipelineConfiguration(pipelineConfiguration)
      }
    }
  }

  /**
   * Get the underlying Receiver (that is, the next one in the pipeline)
   */
  def getUnderlyingReceiver(): Receiver = nextReceiver

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    if (pipelineConfiguration != pipe) {
      pipelineConfiguration = pipe
      if (nextReceiver != null) {
        nextReceiver.setPipelineConfiguration(pipe)
      }
    }
  }

  def getConfiguration(): Configuration = pipelineConfiguration.getConfiguration

  /**
   * Start of event stream
   */
  def open(): Unit = {
    if (nextReceiver == null) {
      throw new IllegalStateException("ProxyReceiver.open(): no underlying receiver provided")
    }
    nextReceiver.open()
  }

  /**
   * End of output. Note that closing this receiver also closes the rest of the
   * pipeline.
   */
  def close(): Unit = {
    nextReceiver.close()
  }

  /**
   * Start of a document node.
   */
  def startDocument(): Unit = {
    nextReceiver.startDocument()
  }

  /**
   * Notify the end of a document node
   */
  def endDocument(): Unit = {
    nextReceiver.endDocument()
  }

  /**
   * Notify the start of an element
   *
   * @param qName   integer code identifying the name of the element within the name pool.
   * @param properties properties of the element node
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    nextReceiver.startElement(qName, properties)
  }

  /**
   * Notify a namespace. Namespaces are notified <b>after</b> the startElement event, and before
   * any children for the element. The namespaces that are reported are only required
   * to include those that are different from the parent element; however, duplicates may be reported.
   * A namespace must not conflict with any namespaces already used for element or attribute names.
   *
   * @param nsBinding an integer: the top half is a prefix code, the bottom half a URI code.
   *                      These may be translated into an actual prefix and URI using the name pool. A prefix code of
   *                      zero represents the empty prefix (that is, the default namespace). A URI code of zero represents
   *                      a URI of "", that is, a namespace undeclaration.
   * @throws IllegalStateException: attempt to output a namespace when there is no open element
   *                                start tag
   */
  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    nextReceiver.namespace(nsBinding, properties)
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
    nextReceiver.attribute(nameCode, value)
  }

  /**
   * Notify the start of the content, that is, the completion of all attributes and namespaces.
   * Note that the initial receiver of output from XSLT instructions will not receive this event,
   * it has to detect it itself. Note that this event is reported for every element even if it has
   * no attributes, no namespaces, and no content.
   */
  def startContent(): Unit = {
    nextReceiver.startContent()
  }

  /**
   * End of element
   */
  def endElement(): Unit = {
    nextReceiver.endElement()
  }

  /**
   * Character data
   */
  def characters(chars: CharSequence): Unit = {
    nextReceiver.characters(chars)
  }

  /**
   * Processing Instruction
   */
  def processingInstruction(target: String, data: CharSequence): Unit = {
    nextReceiver.processingInstruction(target, data)
  }

  /**
   * Output a comment
   */
  def comment(chars: CharSequence): Unit = {
    nextReceiver.comment(chars)
  }

  /**
   * Append an arbitrary item (node or atomic value) to the output
   *
   * @param item           the item to be appended
   * @param copyNamespaces if the item is an element node, this indicates whether its namespaces
   *                       need to be copied. Values are [[org.orbeon.darius.xpath.om.NodeInfo#ALL_NAMESPACES]],
   *                       [[org.orbeon.darius.xpath.om.NodeInfo#LOCAL_NAMESPACES]], [[org.orbeon.darius.xpath.om.NodeInfo#NO_NAMESPACES]]
   */
  def append(item: Item, copyNamespaces: Int): Unit = {
    if (nextReceiver.isInstanceOf[SequenceReceiver]) {
      nextReceiver.asInstanceOf[SequenceReceiver].append(item, copyNamespaces)
    } else {
      throw new UnsupportedOperationException("append() method is not supported in this class")
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.event

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.NamespaceBinding
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * TeeOutputter: a SequenceReceiver that duplicates received events to two different destinations
 */
class TeeOutputter(seq1: Receiver, seq2: Receiver) extends SequenceReceiver {

  var seq1: SequenceReceiver = if (seq1.isInstanceOf[SequenceReceiver]) seq1.asInstanceOf[SequenceReceiver] else new TreeReceiver(seq1)

  var seq2: SequenceReceiver = if (seq2.isInstanceOf[SequenceReceiver]) seq2.asInstanceOf[SequenceReceiver] else new TreeReceiver(seq2)

  /**
   * Output an item (atomic value or node) to the sequence
   */
  def append(item: Item, copyNamespaces: Int): Unit = {
    seq1.append(item, NodeInfo.ALL_NAMESPACES)
    seq2.append(item, NodeInfo.ALL_NAMESPACES)
  }

  /**
   * Notify the start of a document node
   */
  def startDocument(): Unit = {
    seq1.startDocument()
    seq2.startDocument()
  }

  /**
   * Notify the end of a document node
   */
  def endDocument(): Unit = {
    seq1.endDocument()
    seq2.endDocument()
  }

  /**
   * Notify the start of an element
   *
   * @param qName    integer code identifying the name of the element within the name pool.
   * @param properties  bit-significant properties of the element node. If there are no revelant
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    seq1.startElement(qName, properties)
    seq2.startElement(qName, properties)
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
    seq1.namespace(nsBinding, properties)
    seq2.namespace(nsBinding, properties)
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
    seq1.attribute(nameCode, value)
    seq2.attribute(nameCode, value)
  }

  /**
   * Notify the start of the content, that is, the completion of all attributes and namespaces.
   * Note that the initial receiver of output from XSLT instructions will not receive this event,
   * it has to detect it itself. Note that this event is reported for every element even if it has
   * no attributes, no namespaces, and no content.
   */
  def startContent(): Unit = {
    seq1.startContent()
    seq2.startContent()
  }

  /**
   * Notify the end of an element. The receiver must maintain a stack if it needs to know which
   * element is ending.
   */
  def endElement(): Unit = {
    seq1.endElement()
    seq2.endElement()
  }

  /**
   * Notify character data. Note that some receivers may require the character data to be
   * sent in a single event, but in general this is not a requirement.
   *
   * @param chars      The characters
   */
  def characters(chars: CharSequence): Unit = {
    seq1.characters(chars)
    seq2.characters(chars)
  }

  /**
   * Output a processing instruction
   *
   * @param name       The PI name. This must be a legal name (it will not be checked).
   * @param data       The data portion of the processing instruction
   * @throws IllegalArgumentException: the content is invalid for an XML processing instruction
   */
  def processingInstruction(name: String, data: CharSequence): Unit = {
    seq1.processingInstruction(name, data)
    seq2.processingInstruction(name, data)
  }

  /**
   * Notify a comment. Comments are only notified if they are outside the DTD.
   *
   * @param content    The content of the comment
   * @throws IllegalArgumentException: the content is invalid for an XML comment
   */
  def comment(content: CharSequence): Unit = {
    seq1.comment(content)
    seq2.comment(content)
  }

  /**
   * Notify the end of the event stream
   */
  def close(): Unit = {
    seq1.close()
    seq2.close()
  }
}

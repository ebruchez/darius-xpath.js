// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.om.{NamespaceBinding, StructuredQName}

/**
 * Receiver: This interface represents a recipient of XML tree-walking (push) events. It is
 * based on SAX2's ContentHandler, but adapted to handle additional events, and
 * to use Saxon's name pool. Namespaces and Attributes are handled by separate events
 * following the startElement event. Schema types can be defined for elements and attributes.
 * <p>
 * The Receiver interface is an important internal interface within Saxon, and provides a powerful
 * mechanism for integrating Saxon with other applications. It has been designed with extensibility
 * and stability in mind. However, it should be considered as an interface designed primarily for
 * internal use, and not as a completely stable part of the public Saxon API.
 * <p>
 * @author Michael H. Kay
 */
trait Receiver {

  /**
   * Set the pipeline configuration
   * @param pipe the pipeline configuration
   */
  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit

  /**
   * Get the pipeline configuration
   * @return the pipeline configuration
   */
  def getPipelineConfiguration: PipelineConfiguration

  /**
   * Set the System ID of the tree represented by this event stream
   * @param systemId the system ID (which is used as the base URI of the nodes
   * if there is no xml:base attribute)
   */
  def setSystemId(systemId: String): Unit

  /**
   * Notify the start of the event stream
   */
  def open(): Unit

  /**
   * Notify the start of a document node
   */
  def startDocument(): Unit

  /**
   * Notify the end of a document node
   */
  def endDocument(): Unit

  /**
   * Notify the start of an element
   * @param qName the name of the element.
   * @param properties bit-significant properties of the element node. If there are no revelant
   * properties, zero is supplied. The definitions of the bits are in class [[ReceiverOptions]]
   */
  def startElement(qName: StructuredQName, properties: Int): Unit

  /**
   * Notify a namespace. Namespaces are notified <b>after</b> the startElement event, and before
   * any children for the element. The namespaces that are reported are only required
   * to include those that are different from the parent element. The events represent namespace
   * declarations and undeclarations rather than in-scope namespace nodes: an undeclaration is represented
   * by a namespace code of zero. If the sequence of namespace events contains two
   * A namespace must not conflict with any namespaces already used for element or attribute names.
   * @param nsBinding an integer: the top half is a prefix code, the bottom half a URI code.
   * These may be translated into an actual prefix and URI using the name pool. A prefix code of
   * zero represents the empty prefix (that is, the default namespace). A URI code of zero represents
   * a URI of "", that is, a namespace undeclaration.
   * @param properties The most important property is REJECT_DUPLICATES. If this property is set, the
   * namespace declaration will be rejected if it conflicts with a previous declaration of the same
   * prefix. If the property is not set, the namespace declaration will be ignored if it conflicts
   * with a previous declaration. This reflects the fact that when copying a tree, namespaces for child
   * elements are emitted before the namespaces of their parent element. Unfortunately this conflicts
   * with the XSLT rule for complex content construction, where the recovery action in the event of
   * conflicts is to take the namespace that comes last. XSLT therefore doesn't recover from this error:
   */
  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit

  /**
   * Notify an attribute. Attributes are notified after the startElement event, and before any
   * children. Namespaces and attributes may be intermingled.
   *
   * @param nameCode The name of the attribute
   * @param value the string value of the attribute
   * @throws IllegalStateException: attempt to output an attribute when there is no open element
   * start tag
   */
  def attribute(nameCode: StructuredQName, value: CharSequence): Unit

  /**
   * Notify the start of the content, that is, the completion of all attributes and namespaces.
   * Note that the initial receiver of output from XSLT instructions will not receive this event,
   * it has to detect it itself. Note that this event is reported for every element even if it has
   * no attributes, no namespaces, and no content.
   */
  def startContent(): Unit

  /**
   * Notify the end of an element. The receiver must maintain a stack if it needs to know which
   * element is ending.
   */
  def endElement(): Unit

  /**
   * Notify character data. Note that some receivers may require the character data to be
   * sent in a single event, but in general this is not a requirement.
   * @param chars The characters
   */
  def characters(chars: CharSequence): Unit

  /**
   * Output a processing instruction
   * @param name The PI name. This must be a legal name (it will not be checked).
   * @param data The data portion of the processing instruction
   * @throws IllegalArgumentException: the content is invalid for an XML processing instruction
   */
  def processingInstruction(name: String, data: CharSequence): Unit

  /**
   * Notify a comment. Comments are only notified if they are outside the DTD.
   * @param content The content of the comment
   * @throws IllegalArgumentException: the content is invalid for an XML comment
   */
  def comment(content: CharSequence): Unit

  /**
   * Notify the end of the event stream
   */
  def close(): Unit
}

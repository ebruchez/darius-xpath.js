// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.ListIterator
import client.net.sf.saxon.ce.tree.util.Orphan
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.EmptySequence
import client.net.sf.saxon.ce.value.SequenceExtent
import client.net.sf.saxon.ce.Controller
import java.util.ArrayList
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This outputter is used when writing a sequence of atomic values and nodes, that
 * is, when xsl:variable is used with content and an "as" attribute. The outputter
 * builds the sequence and provides access to it. (It isn't really an outputter at all,
 * it doesn't pass the events to anyone, it merely constructs the sequence in memory
 * and provides access to it). Note that the event sequence can include calls such as
 * startElement and endElement that require trees to be built. If nodes such as attributes
 * and text nodes are received while an element is being constructed, the nodes are added
 * to the tree. Otherwise, "orphan" nodes (nodes with no parent) are created and added
 * directly to the sequence.
 *
 * <p>This class is not used to build temporary trees. For that, the ComplexContentOutputter
 * is used.</p>
 *
 *
 * @author Michael H. Kay
 */
class SequenceOutputter(var controller: Controller, estimatedSize: Int) extends SequenceReceiver {

  private var list: ArrayList[Item] = new ArrayList[Item](estimatedSize)

  private var outputter: Receiver = null

  private var builder: Builder = null

  private var level: Int = 0

  private var inStartTag: Boolean = false

  /**
   * Clear the contents of the SequenceOutputter and make it available for reuse
   */
  def reset(): Unit = {
    list = new ArrayList[Item](Math.max(list.size + 10, 50))
    if (controller != null && adviseReuse()) {
      controller.reuseSequenceOutputter(this)
    }
  }

  /**
   * Get the sequence that has been built
   * @return the sequence
   */
  def getSequence(): Sequence = list.size match {
    case 0 ⇒ EmptySequence.getInstance
    case 1 ⇒ list.get(0)
    case _ ⇒ new SequenceExtent(list)
  }

  /**
   * Get an iterator over the sequence of items that has been constructed
   * @return the iterator
   */
  def iterate(): SequenceIterator = {
    if (list.isEmpty) {
      EmptyIterator.getInstance
    } else {
      new ListIterator(list)
    }
  }

  /**
   * Get the first item in the sequence that has been built
   * @return the first item in the list, or null if there are none
   */
  def getFirstItem(): Item = {
    if (list.isEmpty) {
      null
    } else {
      list.get(0)
    }
  }

  /**
   * Start of a document node.
   */
  def startDocument(): Unit = {
    if (outputter == null) {
      createTree()
    }
    if (level += 1 == 0) {
      outputter.startDocument()
    }
  }

  /**
   * Create a tree to hold a document or element node.
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  private def createTree(): Unit = {
    val pipe = getPipelineConfiguration
    builder = pipe.getController.makeBuilder()
    builder.setPipelineConfiguration(pipe)
    builder.setSystemId(getSystemId)
    val reducer = new NamespaceReducer()
    reducer.setUnderlyingReceiver(builder)
    reducer.setPipelineConfiguration(getPipelineConfiguration)
    val cco = new ComplexContentOutputter()
    cco.setPipelineConfiguration(getPipelineConfiguration)
    cco.setReceiver(reducer)
    outputter = cco
    outputter.setSystemId(systemId)
    outputter.setPipelineConfiguration(getPipelineConfiguration)
    outputter.open()
  }

  /**
   * Decide whether reuse of the SequenceWriter is advisable
   * @return true if reuse is considered advisable
   */
  protected def adviseReuse(): Boolean = false

  /**
   * Notify the end of a document node
   */
  def endDocument(): Unit = {
    if (level == 0) {
      outputter.endDocument()
      val doc = builder.getCurrentRoot.asInstanceOf[DocumentInfo]
      append(doc, NodeInfo.ALL_NAMESPACES)
    }
    previousAtomic = false
  }

  /**
   * Output an element start tag.
   * @param qName The element name code - a code held in the Name Pool
   * @param properties bit-significant flags indicating any special information
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    if (inStartTag) {
      startContent()
    }
    if (outputter == null) {
      createTree()
    }
    outputter.startElement(qName, properties)
    level += 1
    inStartTag = true
    previousAtomic = false
  }

  /**
   * Output an element end tag.
   */
  def endElement(): Unit = {
    if (inStartTag) {
      startContent()
    }
    outputter.endElement()
    if (level == 0) {
      outputter.close()
      val element = builder.getCurrentRoot
      append(element, NodeInfo.ALL_NAMESPACES)
    }
    previousAtomic = false
  }

  /**
   * Output a namespace declaration. <br>
   * This is added to a list of pending namespaces for the current start tag.
   * If there is already another declaration of the same prefix, this one is
   * ignored.
   * Note that unlike SAX2 startPrefixMapping(), this call is made AFTER writing the start tag.
   * @param nsBinding The namespace code
   * @param properties Allows special properties to be passed if required
   * @throws client.net.sf.saxon.ce.trans.XPathException if there is no start tag to write to (created using writeStartTag),
   * or if character content has been written since the start tag was written.
   */
  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    if (level == 0) {
      val o = new Orphan()
      o.setNodeKind(Type.NAMESPACE)
      o.setNodeName(new StructuredQName("", "", nsBinding.getPrefix))
      o.setStringValue(nsBinding.getURI)
      append(o, NodeInfo.ALL_NAMESPACES)
    } else {
      outputter.namespace(nsBinding, properties)
    }
    previousAtomic = false
  }

  /**
   * Output an attribute value. <br>
   *
   * @param nameCode An integer code representing the name of the attribute, as held in the Name Pool
   * @param value The value of the attribute
   * @throws client.net.sf.saxon.ce.trans.XPathException if there is no start tag to write to (created using writeStartTag),
   * or if character content has been written since the start tag was written.
   */
  def attribute(nameCode: StructuredQName, value: CharSequence): Unit = {
    if (level == 0) {
      val o = new Orphan()
      o.setNodeKind(Type.ATTRIBUTE)
      o.setNodeName(nameCode)
      o.setStringValue(value)
      append(o, NodeInfo.ALL_NAMESPACES)
    } else {
      outputter.attribute(nameCode, value)
    }
    previousAtomic = false
  }

  /**
   * The startContent() event is notified after all namespaces and attributes of an element
   * have been notified, and before any child nodes are notified.
   * @throws client.net.sf.saxon.ce.trans.XPathException for any failure
   */
  def startContent(): Unit = {
    inStartTag = false
    outputter.startContent()
    previousAtomic = false
  }

  /**
   * Produce text content output. <BR>
   * @param s The String to be output
   * @throws client.net.sf.saxon.ce.trans.XPathException for any failure
   */
  def characters(s: CharSequence): Unit = {
    if (level == 0) {
      val o = new Orphan()
      o.setNodeKind(Type.TEXT)
      o.setStringValue(s.toString)
      append(o, NodeInfo.ALL_NAMESPACES)
    } else {
      if (s.length > 0) {
        if (inStartTag) {
          startContent()
        }
        outputter.characters(s)
      }
    }
    previousAtomic = false
  }

  /**
   * Write a comment.
   */
  def comment(comment: CharSequence): Unit = {
    if (inStartTag) {
      startContent()
    }
    if (level == 0) {
      val o = new Orphan()
      o.setNodeKind(Type.COMMENT)
      o.setStringValue(comment)
      append(o, NodeInfo.ALL_NAMESPACES)
    } else {
      outputter.comment(comment)
    }
    previousAtomic = false
  }

  /**
   * Write a processing instruction
   * No-op in this implementation
   */
  def processingInstruction(target: String, data: CharSequence): Unit = {
    if (inStartTag) {
      startContent()
    }
    if (level == 0) {
      val o = new Orphan()
      o.setNodeName(new StructuredQName("", "", target))
      o.setNodeKind(Type.PROCESSING_INSTRUCTION)
      o.setStringValue(data)
      append(o, NodeInfo.ALL_NAMESPACES)
    } else {
      outputter.processingInstruction(target, data)
    }
    previousAtomic = false
  }

  /**
   * Close the output
   */
  def close(): Unit = {
    previousAtomic = false
    if (outputter != null) {
      outputter.close()
    }
  }

  /**
   * Append an item to the sequence, performing any necessary type-checking and conversion
   */
  def append(item: Item, copyNamespaces: Int): Unit = {
    if (item == null) {
      return
    }
    if (level == 0) {
      list.add(item)
      previousAtomic = false
    } else {
      if (item.isInstanceOf[AtomicValue]) {
        if (previousAtomic) {
          outputter.characters(" ")
        }
        outputter.characters(item.getStringValue)
        previousAtomic = true
      } else {
        item.asInstanceOf[NodeInfo].copy(outputter, CopyOptions.ALL_NAMESPACES | CopyOptions.TYPE_ANNOTATIONS)
        previousAtomic = false
      }
    }
  }
}

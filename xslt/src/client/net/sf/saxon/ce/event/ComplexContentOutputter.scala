// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class is used for generating complex content, that is, the content of an
 * element or document node. It enforces the rules on the order of events within
 * complex content (attributes and namespaces must come first), and it implements
 * part of the namespace fixup rules, in particular, it ensures that there is a
 * namespace node for the namespace used in the element name and in each attribute
 * name.
 *
 * <p>The same ComplexContentOutputter may be used for generating an entire XML
 * document; it is not necessary to create a new outputter for each element node.</p>
 *
 * @author Michael H. Kay
 */
class ComplexContentOutputter extends SequenceReceiver {

  private var nextReceiver: Receiver = _

  private var pendingStartTagDepth: Int = -2

  private var pendingStartTag: StructuredQName = _

  private var level: Int = -1

  private var currentLevelIsDocument: Array[Boolean] = new Array[Boolean](20)

  private var elementIsInNullNamespace: java.lang.Boolean = _

  private var pendingAttCode: Array[StructuredQName] = new Array[StructuredQName](20)

  private var pendingAttValue: Array[String] = new Array[String](20)

  private var pendingAttListSize: Int = 0

  private var pendingNSList: Array[NamespaceBinding] = new Array[NamespaceBinding](20)

  private var pendingNSListSize: Int = 0

  private var startElementProperties: Int = _

  private var declaresDefaultNamespace: Boolean = _

  private var started: Boolean = false

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    if (pipelineConfiguration != pipe) {
      pipelineConfiguration = pipe
      if (nextReceiver != null) {
        nextReceiver.setPipelineConfiguration(pipe)
      }
    }
  }

  /**
   * Set the receiver (to handle the next stage in the pipeline) directly
   * @param receiver the receiver to handle the next stage in the pipeline
   */
  def setReceiver(receiver: Receiver): Unit = {
    this.nextReceiver = receiver
  }

  /**
   * Test whether any content has been written to this ComplexContentOutputter
   * @return true if content has been written
   */
  def contentHasBeenWritten(): Boolean = started

  /**
   * Start the output process
   */
  def open(): Unit = {
    nextReceiver.open()
    previousAtomic = false
  }

  /**
   * Start of a document node.
   */
  def startDocument(): Unit = {
    level += 1
    if (level == 0) {
      nextReceiver.startDocument()
    } else if (pendingStartTagDepth >= 0) {
      startContent()
      pendingStartTagDepth = -2
    }
    previousAtomic = false
    if (currentLevelIsDocument.length < level + 1) {
      val b2 = Array.ofDim[Boolean](level * 2)
      System.arraycopy(currentLevelIsDocument, 0, b2, 0, level)
      currentLevelIsDocument = b2
    }
    currentLevelIsDocument(level) = true
  }

  /**
   * Notify the end of a document node
   */
  def endDocument(): Unit = {
    if (level == 0) {
      nextReceiver.endDocument()
    }
    previousAtomic = false
    level -= 1
  }

  /**
   * Produce text content output. <BR>
   * Special characters are escaped using XML/HTML conventions if the output format
   * requires it.
   * @param s The String to be output
   * @exception XPathException for any failure
   */
  def characters(s: CharSequence): Unit = {
    previousAtomic = false
    if (s == null) return
    val len = s.length
    if (len == 0) return
    if (pendingStartTagDepth >= 0) {
      startContent()
    }
    nextReceiver.characters(s)
  }

  /**
   * Output an element start tag. <br>
   * The actual output of the tag is deferred until all attributes have been output
   * using attribute().
   * @param qName The element name code
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    level += 1
    started = true
    if (pendingStartTagDepth >= 0) {
      startContent()
    }
    startElementProperties = properties
    pendingAttListSize = 0
    pendingNSListSize = 0
    pendingStartTag = qName
    pendingStartTagDepth = 1
    elementIsInNullNamespace = null
    declaresDefaultNamespace = false
    previousAtomic = false
    if (currentLevelIsDocument.length < level + 1) {
      val b2 = Array.ofDim[Boolean](level * 2)
      System.arraycopy(currentLevelIsDocument, 0, b2, 0, level)
      currentLevelIsDocument = b2
    }
    currentLevelIsDocument(level) = false
  }

  /**
   * Output a namespace declaration. <br>
   * This is added to a list of pending namespaces for the current start tag.
   * If there is already another declaration of the same prefix, this one is
   * ignored, unless the REJECT_DUPLICATES flag is set, in which case this is an error.
   * Note that unlike SAX2 startPrefixMapping(), this call is made AFTER writing the start tag.
   * @param nsBinding The namespace code
   * @throws XPathException if there is no start tag to write to (created using writeStartTag),
   * or if character content has been written since the start tag was written.
   */
  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    if (pendingStartTagDepth < 0) {
      throw NoOpenStartTagException.makeNoOpenStartTagException(Type.NAMESPACE, nsBinding.getPrefix, 
        pendingStartTagDepth == -2)
    }
    val rejectDuplicates = (properties & ReceiverOptions.REJECT_DUPLICATES) != 0
    for (i <- 0 until pendingNSListSize) {
      if (nsBinding == pendingNSList(i)) {
        return
      }
      if (nsBinding.getPrefix == pendingNSList(i).getPrefix) {
        if (pendingNSList(i) == NamespaceBinding.DEFAULT_UNDECLARATION || 
          nsBinding == NamespaceBinding.DEFAULT_UNDECLARATION) {
          pendingNSList(i) = nsBinding
        } else if (rejectDuplicates) {
          val prefix = nsBinding.getPrefix
          val uri1 = nsBinding.getURI
          val uri2 = pendingNSList(i).getURI
          val err = new XPathException("Cannot create two namespace nodes with the same prefix mapped to different URIs (prefix=" + 
            (if (prefix.length == 0) "\"\"" else prefix) + 
            ", URI=" + 
            (if (uri1.length == 0) "\"\"" else uri1) + 
            ", URI=" + 
            (if (uri2.length == 0) "\"\"" else uri2) + 
            ")")
          err.setErrorCode("XTDE0430")
          throw err
        } else {
          return
        }
      }
    }
    if (((nsBinding.getPrefix.isEmpty)) && !nsBinding.getURI.isEmpty) {
      declaresDefaultNamespace = true
      if (elementIsInNullNamespace == null) {
        elementIsInNullNamespace = pendingStartTag.getNamespaceURI == NamespaceConstant.NULL
      }
      if (elementIsInNullNamespace) {
        throw new XPathException("Cannot output a namespace node for the default namespace when the element is in no namespace", 
          "XTDE0440")
      }
    }
    if (pendingNSListSize + 1 > pendingNSList.length) {
      val newlist = Array.ofDim[NamespaceBinding](pendingNSListSize * 2)
      System.arraycopy(pendingNSList, 0, newlist, 0, pendingNSListSize)
      pendingNSList = newlist
    }
    pendingNSList(pendingNSListSize += 1) = nsBinding
    previousAtomic = false
  }

  /**
   * Output an attribute value. <br>
   * This is added to a list of pending attributes for the current start tag, overwriting
   * any previous attribute with the same name. <br>
   * This method should NOT be used to output namespace declarations.<br>
   *
   * @param nameCode The name of the attribute
   * @param value The value of the attribute
   * @throws XPathException if there is no start tag to write to (created using writeStartTag),
   * or if character content has been written since the start tag was written.
   */
  def attribute(nameCode: StructuredQName, value: CharSequence): Unit = {
    if (pendingStartTagDepth < 0) {
      throw NoOpenStartTagException.makeNoOpenStartTagException(Type.ATTRIBUTE, nameCode.getDisplayName, 
        level < 0 || currentLevelIsDocument(level))
    }
    for (a <- 0 until pendingAttListSize if pendingAttCode(a) == nameCode) {
      pendingAttValue(a) = value.toString
      return
    }
    if (pendingAttListSize >= pendingAttCode.length) {
      val attCode2 = Array.ofDim[StructuredQName](pendingAttListSize * 2)
      val attValue2 = Array.ofDim[String](pendingAttListSize * 2)
      System.arraycopy(pendingAttCode, 0, attCode2, 0, pendingAttListSize)
      System.arraycopy(pendingAttValue, 0, attValue2, 0, pendingAttListSize)
      pendingAttCode = attCode2
      pendingAttValue = attValue2
    }
    pendingAttCode(pendingAttListSize) = nameCode
    pendingAttValue(pendingAttListSize) = value.toString
    pendingAttListSize += 1
    previousAtomic = false
  }

  /**
   * Check that the prefix for an element or attribute is acceptable, allocating a substitute
   * prefix if not. The prefix is acceptable unless a namespace declaration has been
   * written that assignes this prefix to a different namespace URI. This method
   * also checks that the element or attribute namespace has been declared, and declares it
   * if not.
   * @param nameCode the proposed name, including proposed prefix
   * @param seq sequence number, used for generating a substitute prefix when necessary
   * @return a nameCode to use in place of the proposed nameCode (or the original nameCode
   * if no change is needed)
   */
  private def checkProposedPrefix(nameCode: StructuredQName, seq: Int): StructuredQName = {
    val nsprefix = nameCode.getPrefix
    val nsuri = nameCode.getNamespaceURI
    for (i <- 0 until pendingNSListSize if nsprefix == pendingNSList(i).getPrefix) {
      if ((nsuri == pendingNSList(i).getURI)) {
        return nameCode
      } else {
        val prefix = getSubstitutePrefix(nsprefix, seq)
        val newCode = new StructuredQName(prefix, nsuri, nameCode.getLocalName)
        namespace(new NamespaceBinding(prefix, nsuri), 0)
        return newCode
      }
    }
    namespace(new NamespaceBinding(nsprefix, nsuri), 0)
    nameCode
  }

  /**
   * It is possible for a single output element to use the same prefix to refer to different
   * namespaces. In this case we have to generate an alternative prefix for uniqueness. The
   * one we generate is based on the sequential position of the element/attribute: this is
   * designed to ensure both uniqueness (with a high probability) and repeatability
   * @param prefix the proposed prefix
   * @param seq sequence number for use in the substitute prefix
   * @return a prefix to use in place of the one originally proposed
   */
  private def getSubstitutePrefix(prefix: String, seq: Int): String = prefix + '_' + seq

  /**
   * Output an element end tag.
   */
  def endElement(): Unit = {
    if (pendingStartTagDepth >= 0) {
      startContent()
    } else {
      pendingStartTagDepth = -2
    }
    nextReceiver.endElement()
    level -= 1
    previousAtomic = false
  }

  /**
   * Write a comment
   */
  def comment(comment: CharSequence): Unit = {
    if (pendingStartTagDepth >= 0) {
      startContent()
    }
    nextReceiver.comment(comment)
    previousAtomic = false
  }

  /**
   * Write a processing instruction
   */
  def processingInstruction(target: String, data: CharSequence): Unit = {
    if (pendingStartTagDepth >= 0) {
      startContent()
    }
    nextReceiver.processingInstruction(target, data)
    previousAtomic = false
  }

  /**
   * Append an arbitrary item (node or atomic value) to the output
   * @param item the item to be appended
   * @param copyNamespaces if the item is an element node, this indicates whether its namespaces
   * need to be copied. Values are [[client.net.sf.saxon.ce.om.NodeInfo#ALL_NAMESPACES]],
   * [[client.net.sf.saxon.ce.om.NodeInfo#LOCAL_NAMESPACES]], [[client.net.sf.saxon.ce.om.NodeInfo#NO_NAMESPACES]]
   */
  def append(item: Item, copyNamespaces: Int): Unit = {
    if (item == null) {
    } else if (item.isInstanceOf[AtomicValue]) {
      if (previousAtomic) {
        characters(" ")
      }
      characters(item.getStringValue)
      previousAtomic = true
    } else if (item.asInstanceOf[NodeInfo].getNodeKind == Type.DOCUMENT) {
      startDocument()
      val iter = item.asInstanceOf[NodeInfo].iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
      while (true) {
        val it = iter.next()
        if (it == null) //break
        append(it, copyNamespaces)
      }
      endDocument()
      previousAtomic = false
    } else {
      var copyOptions = CopyOptions.TYPE_ANNOTATIONS
      if (copyNamespaces == NodeInfo.LOCAL_NAMESPACES) {
        copyOptions |= CopyOptions.LOCAL_NAMESPACES
      } else if (copyNamespaces == NodeInfo.ALL_NAMESPACES) {
        copyOptions |= CopyOptions.ALL_NAMESPACES
      }
      item.asInstanceOf[NodeInfo].copy(this, copyOptions)
      previousAtomic = false
    }
  }

  /**
   * Close the output
   */
  def close(): Unit = {
    nextReceiver.close()
    previousAtomic = false
  }

  /**
   * Flush out a pending start tag
   */
  def startContent(): Unit = {
    if (pendingStartTagDepth < 0) {
      return
    }
    started = true
    var props = startElementProperties
    var elcode = pendingStartTag
    if (declaresDefaultNamespace || elcode.getPrefix != "") {
      elcode = checkProposedPrefix(pendingStartTag, 0)
      props = startElementProperties | ReceiverOptions.NAMESPACE_OK
    }
    nextReceiver.startElement(elcode, props)
    for (a <- 0 until pendingAttListSize) {
      val attcode = pendingAttCode(a)
      if (!attcode.getPrefix.isEmpty) {
        pendingAttCode(a) = checkProposedPrefix(attcode, a + 1)
      }
    }
    for (n <- 0 until pendingNSListSize) {
      nextReceiver.namespace(pendingNSList(n), 0)
    }
    for (a <- 0 until pendingAttListSize) {
      nextReceiver.attribute(pendingAttCode(a), pendingAttValue(a))
    }
    nextReceiver.startContent()
    pendingAttListSize = 0
    pendingNSListSize = 0
    pendingStartTagDepth = -1
    pendingStartTag = null
    previousAtomic = false
  }
}

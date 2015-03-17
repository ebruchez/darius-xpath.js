package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Builder
import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import java.util.ArrayList
import LinkedTreeBuilder._
//remove if not needed
import scala.collection.JavaConversions._

object LinkedTreeBuilder {

  private class DefaultNodeFactory extends NodeFactory {

    def makeElementNode(parent: NodeInfo, 
        nameCode: StructuredQName, 
        attlist: AttributeCollection, 
        namespaces: Array[NamespaceBinding], 
        namespacesUsed: Int, 
        pipe: PipelineConfiguration, 
        baseURI: String, 
        sequenceNumber: Int): ElementImpl = {
      val e = new ElementImpl()
      if (namespacesUsed > 0) {
        e.setNamespaceDeclarations(namespaces, namespacesUsed)
      }
      e.initialise(nameCode, attlist, parent, sequenceNumber)
      e.setLocation(baseURI)
      e
    }
  }
}

/**
 * The LinkedTreeBuilder class is responsible for taking a stream of Receiver events and constructing
 * a Document tree using the linked tree implementation.
 * @author Michael H. Kay
 */
class LinkedTreeBuilder extends Builder {

  private var currentNode: ParentNodeImpl = _

  private var contentStarted: Boolean = false

  private var nodeFactory: NodeFactory = new DefaultNodeFactory()

  private var size: Array[Int] = new Array[Int](100)

  private var depth: Int = 0

  private var arrays: ArrayList[Array[NodeImpl]] = new ArrayList[Array[NodeImpl]](20)

  private var elementNameCode: StructuredQName = _

  private var attributes: AttributeCollection = _

  private var namespaces: Array[NamespaceBinding] = _

  private var namespacesUsed: Int = _

  private var allocateSequenceNumbers: Boolean = true

  private var nextNodeNumber: Int = 1

  /**
   * Get the current root node. This will normally be a document node, but if the root of the tree
   * is an element node, it can be an element.
   * @return the root of the tree that is currently being built, or that has been most recently built
   *         using this builder
   */
  def getCurrentRoot(): NodeInfo = {
    val physicalRoot = currentRoot
    if (physicalRoot.isInstanceOf[DocumentImpl] && 
      physicalRoot.asInstanceOf[DocumentImpl].isImaginary) {
      physicalRoot.asInstanceOf[DocumentImpl].getDocumentElement
    } else {
      physicalRoot
    }
  }

  def reset() {
    super.reset()
    currentNode = null
    nodeFactory = null
    depth = 0
    allocateSequenceNumbers = true
    nextNodeNumber = 1
  }

  /**
   * Set whether the builder should allocate sequence numbers to elements as they are added to the
   * tree. This is normally done, because it provides a quick way of comparing document order. But
   * nodes added using XQuery update are not sequence-numbered.
   * @param allocate true if sequence numbers are to be allocated
   */
  def setAllocateSequenceNumbers(allocate: Boolean) {
    allocateSequenceNumbers = allocate
  }

  /**
   * Set the Node Factory to use. If none is specified, the Builder uses its own.
   * @param factory the node factory to be used. This allows custom objects to be used to represent
   * the elements in the tree.
   */
  def setNodeFactory(factory: NodeFactory) {
    nodeFactory = factory
  }

  /**
   * Open the stream of Receiver events
   */
  def open() {
    started = true
    depth = 0
    size(depth) = 0
    super.open()
  }

  /**
   * Start of a document node.
   * This event is ignored: we simply add the contained elements to the current document
   */
  def startDocument() {
    val doc = new DocumentImpl()
    currentRoot = doc
    doc.setSystemId(getSystemId)
    doc.setBaseURI(getBaseURI)
    doc.setConfiguration(config)
    currentNode = doc
    depth = 0
    size(depth) = 0
    doc.setRawSequenceNumber(0)
    contentStarted = true
  }

  /**
   * Notify the end of the document
   */
  def endDocument() {
    currentNode.compact(size(depth))
  }

  /**
   * Close the stream of Receiver events
   */
  def close() {
    if (currentNode == null) {
      return
    }
    currentNode.compact(size(depth))
    currentNode = null
    arrays = null
    super.close()
    nodeFactory = null
  }

  /**
   * Notify the start of an element
   */
  def startElement(qName: StructuredQName, properties: Int) {
    if (currentNode == null) {
      startDocument()
      currentRoot.asInstanceOf[DocumentImpl].setImaginary(true)
    }
    elementNameCode = qName
    namespacesUsed = 0
    attributes = null
    contentStarted = false
  }

  def namespace(nsBinding: NamespaceBinding, properties: Int) {
    if (contentStarted) {
      throw new IllegalStateException("namespace() called after startContent()")
    }
    if (namespaces == null) {
      namespaces = Array.ofDim[NamespaceBinding](5)
    }
    if (namespacesUsed == namespaces.length) {
      val ns2 = Array.ofDim[NamespaceBinding](namespaces.length * 2)
      System.arraycopy(namespaces, 0, ns2, 0, namespacesUsed)
      namespaces = ns2
    }
    namespaces(namespacesUsed += 1) = nsBinding
  }

  def attribute(nameCode: StructuredQName, value: CharSequence) {
    if (contentStarted) {
      throw new IllegalStateException("attribute() called after startContent()")
    }
    if (attributes == null) {
      attributes = new AttributeCollection()
    }
    attributes.addAttribute(nameCode, value.toString)
  }

  def startContent() {
    if (contentStarted) {
      throw new IllegalStateException("startContent() called more than once")
    }
    contentStarted = true
    if (attributes == null) {
      attributes = AttributeCollection.EMPTY_ATTRIBUTE_COLLECTION
    } else {
      attributes.compact()
    }
    var nslist = namespaces
    if (nslist == null || namespacesUsed == 0) {
      nslist = NamespaceBinding.EMPTY_ARRAY
    }
    val elem = nodeFactory.makeElementNode(currentNode, elementNameCode, attributes, nslist, namespacesUsed, 
      pipe, getSystemId, (if (allocateSequenceNumbers) nextNodeNumber += 1 else -1))
    namespacesUsed = 0
    attributes = null
    while (depth >= arrays.size) {
      arrays.add(Array.ofDim[NodeImpl](20))
    }
    elem.setChildren(arrays.get(depth))
    currentNode.addChild(elem, size(depth) += 1)
    if (depth >= size.length - 1) {
      val newsize = Array.ofDim[Int](size.length * 2)
      System.arraycopy(size, 0, newsize, 0, size.length)
      size = newsize
    }
    size(depth) = 0
    namespacesUsed = 0
    if (currentNode.isInstanceOf[DocumentInfo]) {
      currentNode.asInstanceOf[DocumentImpl].setDocumentElement(elem)
    }
    currentNode = elem
  }

  /**
   * Notify the end of an element
   */
  def endElement() {
    if (!contentStarted) {
      throw new IllegalStateException("missing call on startContent()")
    }
    currentNode.compact(size(depth))
    depth -= 1
    currentNode = currentNode.getParent.asInstanceOf[ParentNodeImpl]
  }

  /**
   * Notify a text node. Adjacent text nodes must have already been merged
   */
  def characters(chars: CharSequence) {
    if (!contentStarted) {
      throw new IllegalStateException("missing call on startContent()")
    }
    if (chars.length > 0) {
      val prev = currentNode.getNthChild(size(depth) - 1)
      if (prev.isInstanceOf[TextImpl]) {
        prev.asInstanceOf[TextImpl].appendStringValue(chars.toString)
      } else {
        val n = new TextImpl(chars.toString)
        currentNode.addChild(n, size(depth) += 1)
      }
    }
  }

  /**
   * Notify a processing instruction
   */
  def processingInstruction(name: String, remainder: CharSequence) {
    if (!contentStarted) {
      throw new IllegalStateException("missing call on startContent()")
    }
    val pi = new ProcInstImpl(name, remainder.toString)
    currentNode.addChild(pi, size(depth) += 1)
  }

  /**
   * Notify a comment
   */
  def comment(chars: CharSequence) {
    if (!contentStarted) {
      throw new IllegalStateException("missing call on startContent()")
    }
    val comment = new CommentImpl(chars.toString)
    currentNode.addChild(comment, size(depth) += 1)
  }

  /**
   * Get the current document or element node
   * @return the most recently started document or element node (to which children are currently being added)
   * In the case of elements, this is only available after startContent() has been called
   */
  def getCurrentParentNode(): ParentNodeImpl = currentNode

  /**
   * Get the current text, comment, or processing instruction node
   * @return if any text, comment, or processing instruction nodes have been added to the current parent
   * node, then return that text, comment, or PI; otherwise return null
   */
  def getCurrentLeafNode(): NodeImpl = {
    currentNode.getLastChild.asInstanceOf[NodeImpl]
  }

  /**
   * graftElement() allows an element node to be transferred from one tree to another.
   * This is a dangerous internal interface which is used only to contruct a stylesheet
   * tree from a stylesheet using the "literal result element as stylesheet" syntax.
   * The supplied element is grafted onto the current element as its only child.
   * @param element the element to be grafted in as a new child.
   */
  def graftElement(element: ElementImpl) {
    currentNode.addChild(element, size(depth) += 1)
  }
}

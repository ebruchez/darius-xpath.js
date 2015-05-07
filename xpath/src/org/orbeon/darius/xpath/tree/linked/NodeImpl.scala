// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.linked

import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.event.Builder
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.pattern.AnyNodeTest
import org.orbeon.darius.xpath.pattern.NameTest
import org.orbeon.darius.xpath.pattern.NodeTest
import org.orbeon.darius.xpath.tree.NamespaceNode
import org.orbeon.darius.xpath.tree.iter._
import org.orbeon.darius.xpath.tree.linked.NodeImpl._
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.tree.util.Navigator
import org.orbeon.darius.xpath.value.AbstractNode
import org.orbeon.darius.xpath.value.AtomicValue
import org.orbeon.darius.xpath.value.UntypedAtomicValue

object NodeImpl {

  /**
   * Chararacteristic letters to identify each type of node, indexed using the node type
   * values. These are used as the initial letter of the result of generate-id()
   */
  val NODE_LETTER = Array('x', 'e', 'a', 't', 'x', 'x', 'x', 'p', 'c', 'r', 'x', 'x', 'x', 'n')

  private class NextDescendantFunction(var anchor: NodeImpl, var predicate: NodeTest)
      extends SteppingIterator.SteppingFunction {

    def step(current: Item): Item = {
      current.asInstanceOf[NodeImpl].getNextInDocument(anchor)
    }

    def conforms(current: Item): Boolean = predicate.matchesItem(current)
  }

  private class PrecedingSiblingFunction(var predicate: NodeTest) extends SteppingIterator.SteppingFunction {

    def step(current: Item): Item = {
      current.asInstanceOf[NodeImpl].getPreviousSibling
    }

    def conforms(current: Item): Boolean = predicate.matchesItem(current)
  }

  private class NextSiblingFunction(var predicate: NodeTest) extends SteppingIterator.SteppingFunction {

    def step(current: Item): Item = {
      current.asInstanceOf[NodeImpl].getNextSibling
    }

    def conforms(current: Item): Boolean = predicate.matchesItem(current)
  }
}

/**
 * A node in the "linked" tree representing any kind of node except a namespace node.
 * Specific node kinds are represented by concrete subclasses.
 *
 * @author Michael H. Kay
 */
abstract class NodeImpl extends AbstractNode with NodeInfo {

  private var parent: ParentNodeImpl = _

  private var index: Int = _

  /**
   * Get the document number of the document containing this node. For a free-standing
   * orphan node, just return the hashcode.
   */
  def getDocumentNumber: Int = getPhysicalRoot.getDocumentNumber

  /**
   * Get the index position of this node among its siblings (starting from 0)
   * @return 0 for the first child, 1 for the second child, etc. Returns -1 for a node
   * that has been deleted.
   */
  def getSiblingPosition: Int = index

  /**
   * Set the index position. For internal use only
   * @param index the position of the node among its siblings, counting from zero.
   */
  protected[linked] def setSiblingPosition(index: Int): Unit = {
    this.index = index
  }

  /**
   * Get the typed value of this node.
   * If there is no type annotation, we return the string value, as an instance
   * of xs:untypedAtomic
   */
  def getTypedValue: AtomicValue = new UntypedAtomicValue(getStringValue)

  /**
   * Set the system ID of this node. This method is provided so that a NodeInfo
   * implements the javax.xml.transform.Source interface, allowing a node to be
   * used directly as the Source of a transformation
   */
  def setSystemId(uri: String): Unit = {
    getParent.asInstanceOf[NodeImpl].setSystemId(uri)
  }

  /**
   * Determine whether this is the same node as another node
   *
   * @return true if this Node object and the supplied Node object represent the
   *         same node in the tree.
   */
  def isSameNodeInfo(other: NodeInfo): Boolean = this eq other

  /**
   * The equals() method compares nodes for identity. It is defined to give the same result
   * as isSameNodeInfo().
   * @param other the node to be compared with this node
   * @return true if this NodeInfo object and the supplied NodeInfo object represent
   *      the same node in the tree.
   * @since 8.7 Previously, the effect of the equals() method was not defined. Callers
   * should therefore be aware that third party implementations of the NodeInfo interface may
   * not implement the correct semantics. It is safer to use isSameNodeInfo() for this reason.
   * The equals() method has been defined because it is useful in contexts such as a Java Set or HashMap.
   */
  override def equals(other: Any): Boolean = other match {
    case other: NodeInfo ⇒ isSameNodeInfo(other)
    case _ ⇒ false
  }

  /**
   * Get the name of the node
   *
   * @return the name of the node, as a StructuredQName. Return null for an unnamed node.
   */
  def getNodeName: StructuredQName = null

  /**
   * Get a character string that uniquely identifies this node within this document
   * (The calling code will prepend a document identifier)
   */
  def generateId(buffer: FastStringBuffer): Unit = {
    parent.generateId(buffer)
    buffer.append(NODE_LETTER(getNodeKind))
    buffer.append(Integer toString index)
  }

  /**
   * Get the system ID for the node. Default implementation for child nodes.
   */
  def getSystemId: String = parent.getSystemId

  /**
   * Get the base URI for the node. Default implementation for child nodes.
   */
  def getBaseURI: String = parent.getBaseURI

  /**
   * Get the node sequence number (in document order). Sequence numbers are monotonic but not
   * consecutive. In the current implementation, parent nodes (elements and roots) have a zero
   * least-significant word, while namespaces, attributes, text nodes, comments, and PIs have
   * the top word the same as their owner and the bottom half reflecting their relative position.
   * This is the default implementation for child nodes.
   * For nodes added by XQuery Update, the sequence number is -1L
   * @return the sequence number if there is one as an array containing two integers
   */
  protected def getSequenceNumber: Array[Int] = {
    var prev = this
    var i = 0
    while (true) {
      if (prev.isInstanceOf[ParentNodeImpl]) {
        val prevseq = prev.getSequenceNumber
        return Array(prevseq(0), prevseq(1) + 0x10000 + i)
      }
      prev = prev.getPreviousInDocument
      i += 1
    }
    throw new IllegalStateException
  }

  /**
   * Determine the relative position of this node and another node, in document order.
   * The other node will always be in the same document.
   *
   * @param other The other node, whose position is to be compared with this node
   * @return -1 if this node precedes the other node, +1 if it follows the other
   *         node, or 0 if they are the same node. (In this case, isSameNode() will always
   *         return true, and the two nodes will produce the same result for generateId())
   */
  def compareOrder(other: NodeInfo): Int = {
    if (other.isInstanceOf[NamespaceNode]) {
      return 0 - other.compareOrder(this)
    }
    val a = getSequenceNumber
    val b = other.asInstanceOf[NodeImpl].getSequenceNumber
    if (a(0) < b(0)) {
      return -1
    }
    if (a(0) > b(0)) {
      return +1
    }
    if (a(1) < b(1)) {
      return -1
    }
    if (a(1) > b(1)) {
      return +1
    }
    0
  }

  /**
   * Get the configuration
   */
  def getConfiguration: Configuration = getPhysicalRoot.getConfiguration

  /**
   * Get the URI part of the name of this node. This is the URI corresponding to the
   * prefix, or the URI of the default namespace if appropriate.
   *
   * @return The URI of the namespace of this node. For the null namespace, return an
   *         empty string. For an unnamed node, return the empty string.
   */
  def getURI: String = {
    val qName = getNodeName
    if (qName eq null) "" else qName.getNamespaceURI
  }

  /**
   * Get the display name of this node. For elements and attributes this is [prefix:]localname.
   * For unnamed nodes, it is an empty string.
   *
   * @return The display name of this node.
   *         For a node with no name, return an empty string.
   */
  def getDisplayName: String = {
    val qName = getNodeName
    if (qName eq null) "" else qName.getDisplayName
  }

  /**
   * Get the local name of this node.
   *
   * @return The local name of this node.
   *         For a node with no name, return "",.
   */
  def getLocalPart: String = {
    val qName = getNodeName
    if (qName eq null) "" else qName.getLocalName
  }

  /**
   * Find the parent node of this node.
   *
   * @return The Node object describing the containing element or root node.
   */
  def getParent: NodeInfo =
    parent match {
      case documentImpl: DocumentImpl if documentImpl.isImaginary ⇒ null
      case _                                                      ⇒ parent
    }

  /**
   * Get the raw value of the parent pointer. This will usually be the same as the parent node
   * in the XDM model, but in the case of a parentless element it will be a pointer to the "imaginary"
   * document node which is not properly part of the tree.
   */
  protected def getRawParent: ParentNodeImpl = parent

  /**
   * Set the raw parent pointer
   */
  protected[linked] def setRawParent(parent: ParentNodeImpl): Unit = {
    this.parent = parent
  }

  /**
   * Get the previous sibling of the node
   *
   * @return The previous sibling node. Returns null if the current node is the first
   *         child of its parent.
   */
  def getPreviousSibling: NodeInfo = {
    if (parent eq null) {
      return null
    }
    parent.getNthChild(index - 1)
  }

  /**
   * Get next sibling node
   *
   * @return The next sibling node of the required type. Returns null if the current node is the last
   *         child of its parent.
   */
  def getNextSibling: NodeInfo = {
    if (parent eq null) {
      return null
    }
    parent.getNthChild(index + 1)
  }

  /**
   * Get first child - default implementation used for leaf nodes
   *
   * @return null
   */
  def getFirstChild: NodeInfo = null

  /**
   * Get last child - default implementation used for leaf nodes
   *
   * @return null
   */
  def getLastChild: NodeInfo = null

  /**
   * Return an enumeration over the nodes reached by the given axis from this node
   *
   * @param axisNumber The axis to be iterated over
   * @param nodeTest   A pattern to be matched by the returned nodes
   * @return an AxisIterator that scans the nodes reached by the axis in turn.
   */
  def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = axisNumber match {
    case Axis.ANCESTOR ⇒ 
      new SteppingIterator(this, new Navigator.ParentFunction(nodeTest), false)
    case Axis.ANCESTOR_OR_SELF ⇒ 
      new SteppingIterator(this, new Navigator.ParentFunction(nodeTest), true)
    case Axis.ATTRIBUTE ⇒
      if (getNodeKind != Type.ELEMENT) {
        EmptyIterator.getInstance
      } else {
        val atts = this.asInstanceOf[ElementImpl].getAttributeList
        nodeTest match {
          case nameTest: NameTest ⇒
            val index = atts.findByStructuredQName(nameTest.getRequiredNodeName)
            if (index < 0) {
              EmptyIterator.getInstance
            } else {
              val a = new AttributeImpl(this.asInstanceOf[ElementImpl], index)
              SingletonIterator.makeIterator(a)
            }
          case _ ⇒
            val nodes = new Array[AttributeImpl](atts.getLength)
            for (i ← 0 until atts.getLength) {
              nodes(i) = new AttributeImpl(this.asInstanceOf[ElementImpl], i)
            }
            Navigator.newAxisFilter(new ArrayIterator(nodes), nodeTest)
        }
      }
    case Axis.CHILD ⇒
      this match {
        case parentNodeImpl: ParentNodeImpl ⇒
          val all = new ArrayIterator(parentNodeImpl.allChildren())
          if (nodeTest == AnyNodeTest.getInstance) {
            all
          } else {
            Navigator.newAxisFilter(all, nodeTest)
          }
        case _ ⇒
          EmptyIterator.getInstance
      }
    case Axis.DESCENDANT ⇒
      if (getNodeKind == Type.DOCUMENT && nodeTest.isInstanceOf[NameTest] &&
        nodeTest.getRequiredNodeKind == Type.ELEMENT) {
        this.asInstanceOf[DocumentImpl].getAllElements(nodeTest.asInstanceOf[NameTest].getRequiredNodeName)
      } else if (hasChildNodes) {
        new SteppingIterator(this, new NextDescendantFunction(this, nodeTest), false)
      } else {
        EmptyIterator.getInstance
      }
    case Axis.DESCENDANT_OR_SELF ⇒
      new SteppingIterator(this, new NextDescendantFunction(this, nodeTest), true)
    case Axis.FOLLOWING ⇒ 
      Navigator.newAxisFilter(new Navigator.FollowingEnumeration(this), nodeTest)
    case Axis.FOLLOWING_SIBLING ⇒ 
      new SteppingIterator(this, new NextSiblingFunction(nodeTest), false)
    case Axis.NAMESPACE ⇒
      if (getNodeKind != Type.ELEMENT) {
        EmptyIterator.getInstance
      } else {
        NamespaceNode.makeIterator(this, nodeTest)
      }
    case Axis.PARENT ⇒
      val parent = getParent
      if (parent eq null) {
        EmptyIterator.getInstance
      } else {
        Navigator.filteredSingleton(parent, nodeTest)
      }
    case Axis.PRECEDING ⇒ 
      Navigator.newAxisFilter(new Navigator.PrecedingEnumeration(this, false), nodeTest)
    case Axis.PRECEDING_SIBLING ⇒ 
      new SteppingIterator(this, new PrecedingSiblingFunction(nodeTest), false)
    case Axis.SELF ⇒ 
      Navigator.filteredSingleton(this, nodeTest)
    case _ ⇒ 
      throw new IllegalArgumentException("Unknown axis number " + axisNumber)
  }

  /**
   * Get the root node
   * @return the NodeInfo representing the logical root of the tree. For this tree implementation the
   * root will either be a document node or an element node.
   */
  def getRoot: NodeInfo = {
    val parent = getParent
    if (parent eq null) {
      this
    } else {
      parent.getRoot
    }
  }

  /**
   * Get the root (document) node
   * @return the DocumentInfo representing the containing document. If this
   *     node is part of a tree that does not have a document node as its
   *     root, returns null.
   */
  def getDocumentRoot: DocumentInfo = {
    val parent = getParent
    if (parent eq null) {
      null
    } else {
      parent.getDocumentRoot
    }
  }

  /**
   * Get the physical root of the tree. This may be an imaginary document node: this method
   * should be used only when control information held at the physical root is required
   * @return the document node, which may be imaginary. In the case of a node that has been detached
   * from the tree by means of a delete() operation, this method returns null.
   */
  def getPhysicalRoot: DocumentImpl = {
    var up = parent
    while ((up ne null) && ! up.isInstanceOf[DocumentImpl]) {
      up = up.getRawParent
    }
    up.asInstanceOf[DocumentImpl]
  }

  /**
   * Get the next node in document order
   *
   * @param anchor the scan stops when it reaches a node that is not a descendant of the specified
   *               anchor node
   * @return the next node in the document, or null if there is no such node
   */
  def getNextInDocument(anchor: NodeImpl): NodeImpl = {
    var next = getFirstChild.asInstanceOf[NodeImpl]
    if (next ne null) {
      return next
    }
    if (this eq anchor) {
      return null
    }
    next = getNextSibling.asInstanceOf[NodeImpl]
    if (next ne null) {
      return next
    }
    var parent = this
    while (true) {
      parent = parent.getParent.asInstanceOf[NodeImpl]
      if (parent eq null) {
        return null
      }
      if (parent eq anchor) {
        return null
      }
      next = parent.getNextSibling.asInstanceOf[NodeImpl]
      if (next ne null) {
        return next
      }
    }
    throw new IllegalStateException
  }

  /**
   * Get the previous node in document order
   *
   * @return the previous node in the document, or null if there is no such node
   */
  def getPreviousInDocument: NodeImpl = {
    val prev = getPreviousSibling.asInstanceOf[NodeImpl]
    if (prev ne null) {
      return prev.getLastDescendantOrSelf
    }
    getParent.asInstanceOf[NodeImpl]
  }

  private def getLastDescendantOrSelf: NodeImpl = {
    val last = getLastChild.asInstanceOf[NodeImpl]
    if (last eq null) {
      return this
    }
    last.getLastDescendantOrSelf
  }

  /**
   * Get all namespace undeclarations and undeclarations defined on this element.
   *
   * @param buffer If this is non-null, and the result array fits in this buffer, then the result
   *               may overwrite the contents of this array, to avoid the cost of allocating a new array on the heap.
   * @return An array of integers representing the namespace declarations and undeclarations present on
   *         this element. For a node other than an element, return null. Otherwise, the returned array is a
   *         sequence of namespace codes, whose meaning may be interpreted by reference to the name pool. The
   *         top half word of each namespace code represents the prefix, the bottom half represents the URI.
   *         If the bottom half is zero, then this is a namespace undeclaration rather than a declaration.
   *         The XML namespace is never included in the list. If the supplied array is larger than required,
   *         then the first unused entry will be set to -1.
   *         
   *         For a node other than an element, the method returns null.
   */
  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

  /**
   * Determine whether the node has any children.
   *
   * @return <code>true</code> if the node has any children,
   *         <code>false</code> if the node has no children.
   */
  def hasChildNodes: Boolean = getFirstChild ne null

  /**
   * Get a Builder suitable for building nodes that can be attached to this document.
   * @return a new Builder that constructs nodes using the same object model implementation
   * as this one, suitable for attachment to this tree
   */
  def newBuilder(): Builder = getPhysicalRoot.newBuilder()
}

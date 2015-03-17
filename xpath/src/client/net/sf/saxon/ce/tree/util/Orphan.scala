// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.util

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.NodeTest
import client.net.sf.saxon.ce.tree.iter.{EmptyIterator, UnfailingIterator}
import client.net.sf.saxon.ce.value.{AbstractNode, AtomicValue, StringValue, UntypedAtomicValue}

import scala.beans.BeanProperty

/**
 * A node (implementing the NodeInfo interface) representing an attribute, text node,
 * comment, processing instruction, or namespace that has no parent (and of course no children).
 * Exceptionally it is also used (during whitespace stripping) to represent a standalone element.
 *
 * <p>In general this class does not impose constraints defined in the data model: that is the responsibility
 * of the client. For example, the class does not prevent you from creating a comment or text node that has
 * a name or a non-trivial type annotation.</p>
 *
 * @author Michael H. Kay
 */
class Orphan extends AbstractNode with NodeInfo {

  private var kind: Int = _

  private var qName: StructuredQName = null

  private var stringValue: CharSequence = _

  @BeanProperty
  var systemId: String = _

  /**
   * Set the node kind
   * @param kind the kind of node, for example [[Type.ELEMENT]] or [[Type.ATTRIBUTE]]
   */
  def setNodeKind(kind: Int) {
    this.kind = kind
  }

  /**
   * Set the name of the node
   * @param nameCode the the name of the node
   */
  def setNodeName(nameCode: StructuredQName) {
    this.qName = nameCode
  }

  /**
   * Set the string value of the node
   * @param stringValue the string value of the node
   */
  def setStringValue(stringValue: CharSequence) {
    this.stringValue = stringValue
  }

  /**
   * Return the kind of node.
   * @return one of the values Type.ELEMENT, Type.TEXT, Type.ATTRIBUTE, etc.
   */
  def getNodeKind(): Int = kind

  /**
   * Get the typed value of the node
   * @return an iterator over the items making up the typed value
   */
  def getTypedValue(): AtomicValue = getNodeKind match {
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION => new StringValue(stringValue)
    case _ => new UntypedAtomicValue(stringValue)
  }

  /**
   * Determine whether this is the same node as another node. <br />
   * Note: a.isSameNode(b) if and only if generateId(a)==generateId(b)
   * @return true if this Node object and the supplied Node object represent the
   * same node in the tree.
   */
  def isSameNodeInfo(other: NodeInfo): Boolean = this == other

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
    case other: NodeInfo => isSameNodeInfo(other)
    case _ => false
  }

  /**
   * The hashCode() method obeys the contract for hashCode(): that is, if two objects are equal
   * (represent the same node) then they must have the same hashCode()
   * @since 8.7 Previously, the effect of the equals() and hashCode() methods was not defined. Callers
   * should therefore be aware that third party implementations of the NodeInfo interface may
   * not implement the correct semantics.
   */
  override def hashCode(): Int = super.hashCode

  /**
   * Get the Base URI for the node, that is, the URI used for resolving a relative URI contained
   * in the node. This will be the same as the System ID unless xml:base has been used.
   */
  def getBaseURI(): String = {
    if (kind == Type.PROCESSING_INSTRUCTION) {
      systemId
    } else {
      null
    }
  }

  /**
   * Determine the relative position of this node and another node, in document order.
   * The other node will always be in the same document.
   * @param other The other node, whose position is to be compared with this node
   * @return -1 if this node precedes the other node, +1 if it follows the other
   * node, or 0 if they are the same node. (In this case, isSameNode() will always
   * return true, and the two nodes will produce the same result for generateId())
   */
  def compareOrder(other: NodeInfo): Int = {
    if (this.isSameNodeInfo(other)) {
      return 0
    }
    if (this.hashCode < other.hashCode) -1 else +1
  }

  /**
   * Get the index position of this node among its siblings (starting from 0)
   *
   * @return 0 for the first child, 1 for the second child, etc.
   */
  def getSiblingPosition(): Int = 1

  /**
   * Return the string value of the node.
   * @return the string value of the node
   */
  def getStringValue(): String = stringValue.toString

  /**
   * Get the name of the node
   *
   * @return the name of the node, as a StructuredQName. Return null for an unnamed node.
   */
  def getNodeName(): StructuredQName = qName

  /**
   * Get the local part of the name of this node. This is the name after the ":" if any.
   * @return the local part of the name. For an unnamed node, returns "".
   */
  def getLocalPart(): String = {
    if (qName == null) "" else qName.getLocalName
  }

  /**
   * Get the URI part of the name of this node. This is the URI corresponding to the
   * prefix, or the URI of the default namespace if appropriate.
   * @return The URI of the namespace of this node. For an unnamed node, return null.
   * For a node with an empty prefix, return an empty string.
   */
  def getURI(): String = {
    if (qName == null) "" else qName.getNamespaceURI()
  }

  /**
   * Get the display name of this node. For elements and attributes this is [prefix:]localname.
   * For unnamed nodes, it is an empty string.
   * @return The display name of this node.
   * For a node with no name, return an empty string.
   */
  def getDisplayName(): String = {
    if (qName == null) "" else qName.getDisplayName()
  }

  /**
   * Get the NodeInfo object representing the parent of this node
   * @return null - an Orphan has no parent.
   */
  def getParent(): NodeInfo = null

  /**
   * Return an iteration over the nodes reached by the given axis from this node
   * @param axisNumber the axis to be searched, e.g. Axis.CHILD or Axis.ANCESTOR
   * @param nodeTest A pattern to be matched by the returned nodes
   * @return a SequenceIterator that scans the nodes reached by the axis in turn.
   */
  def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = axisNumber match {
    case Axis.ANCESTOR_OR_SELF | Axis.DESCENDANT_OR_SELF | Axis.SELF => Navigator.filteredSingleton(this, 
      nodeTest)
    case Axis.ANCESTOR | Axis.ATTRIBUTE | Axis.CHILD | Axis.DESCENDANT | Axis.FOLLOWING | Axis.FOLLOWING_SIBLING | Axis.NAMESPACE | Axis.PARENT | Axis.PRECEDING | Axis.PRECEDING_SIBLING => EmptyIterator.getInstance
    case _ => throw new IllegalArgumentException("Unknown axis number " + axisNumber)
  }

  /**
   * Get the root node of this tree (not necessarily a document node).
   * Always returns this node in the case of an Orphan node.
   */
  def getRoot(): NodeInfo = this

  /**
   * Get the root (document) node
   * @return the DocumentInfo representing the containing document, or null if the
   * node is not part of a document. Always null for an Orphan node.
   */
  def getDocumentRoot(): DocumentInfo = null

  /**
   * Determine whether the node has any children.
   * @return false - an orphan node never has any children
   */
  def hasChildNodes(): Boolean = false

  /**
   * Get a character string that uniquely identifies this node.
   * Note: a.isSameNode(b) if and only if generateId(a)==generateId(b)
   * @param buffer a buffer, into which will be placed
   * a string that uniquely identifies this node, within this
   * document. The calling code prepends information to make the result
   * unique across all documents.
   */
  def generateId(buffer: FastStringBuffer) {
    buffer.append('Q')
    buffer.append(Integer toString hashCode)
  }

  /**
   * Get the document number of the document containing this node. For a free-standing
   * orphan node, just return the hashcode.
   */
  def getDocumentNumber(): Int = hashCode & 0xffffff

  /**
   * Copy this node to a given outputter (deep copy)
   */
  def copy(out: Receiver, copyOptions: Int) {
    ???
//ORBEON unused
//    Navigator.copy(this, out, copyOptions)
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
   *         <p/>
   *         <p>For a node other than an element, the method returns null.</p>
   */
  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.tree.util.FastStringBuffer

/**
 * A node in the "linked" tree representing an attribute. Note that this is
 * generated only "on demand", when the attribute is selected by a path expression.
 *
 * It is possible for multiple AttributeImpl objects to represent the same attribute node.
 * The identity of an attribute node is determined by the identity of the element, and the index
 * position of the attribute within the element. Index positions are not reused when an attribute
 * is deleted, and are retained when an attribute is renamed.
 *
 * This object no longer caches information such as the name code and string value, because
 * these would become invalid when the element node is modified.
 *
 * @author Michael H. Kay
 */
class AttributeImpl(element: ElementImpl, index: Int) extends NodeImpl {

  setRawParent(element)

  setSiblingPosition(index)

  /**
   * Get the name of the node
   */
  override def getNodeName: StructuredQName = {
    if ((getRawParent eq null) || getSiblingPosition == -1) {
      return null
    }
    getRawParent.asInstanceOf[ElementImpl].getAttributeList
      .getStructuredQName(getSiblingPosition)
  }

  /**
   * Determine whether this is the same node as another node
   * @return true if this Node object and the supplied Node object represent the
   * same node in the tree.
   */
  override def isSameNodeInfo(other: NodeInfo): Boolean = {
    if (! other.isInstanceOf[AttributeImpl]) {
      return false
    }
    if (this eq other) {
      return true
    }
    val otherAtt = other.asInstanceOf[AttributeImpl]
    getRawParent.isSameNodeInfo(otherAtt.getRawParent) && getSiblingPosition == otherAtt.getSiblingPosition
  }

  /**
   * The hashCode() method obeys the contract for hashCode(): that is, if two objects are equal
   * (represent the same node) then they must have the same hashCode()
   * @since 8.7 Previously, the effect of the equals() and hashCode() methods was not defined. Callers
   * should therefore be aware that third party implementations of the NodeInfo interface may
   * not implement the correct semantics.
   */
  override def hashCode(): Int = {
    getRawParent.hashCode ^ (getSiblingPosition << 16)
  }

  /**
   * Get the node sequence number (in document order). Sequence numbers are monotonic but not
   * consecutive. In the current implementation, parent nodes (elements and roots) have a zero
   * least-significant word, while namespaces, attributes, text nodes, comments, and PIs have
   * the top word the same as their owner and the bottom half reflecting their relative position.
   */
  override protected def getSequenceNumber: Array[Int] = {
    Array(getRawParent.getRawSequenceNumber, 0x8000 + getSiblingPosition)
  }

  /**
   * Return the type of node.
   * @return Node.ATTRIBUTE
   */
  def getNodeKind: Int = Type.ATTRIBUTE

  /**
   * Return the character value of the node.
   * @return the attribute value
   */
  def getStringValue: String = {
    getRawParent.asInstanceOf[ElementImpl].getAttributeList
      .getValue(getSiblingPosition)
  }

  /**
   * Get next sibling - not defined for attributes
   */
  override def getNextSibling: NodeInfo = null

  /**
   * Get previous sibling - not defined for attributes
   */
  override def getPreviousSibling: NodeInfo = null

  /**
   * Get the previous node in document order (skipping attributes)
   */
  override def getPreviousInDocument: NodeImpl = getParent.asInstanceOf[NodeImpl]

  /**
   * Get the next node in document order (skipping attributes)
   */
  override def getNextInDocument(anchor: NodeImpl): NodeImpl = {
    if (anchor eq this) return null
    getParent.asInstanceOf[NodeImpl].getNextInDocument(anchor)
  }

  /**
   * Get sequential key. Returns key of owning element with the attribute index as a suffix
   * @param buffer a buffer to which the generated ID will be written
   */
  override def generateId(buffer: FastStringBuffer): Unit = {
    getParent.generateId(buffer)
    buffer.append('a')
    buffer.append(Integer toString getSiblingPosition)
  }

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int): Unit = {
    val name = getNodeName
    out.attribute(name, getStringValue)
  }
}

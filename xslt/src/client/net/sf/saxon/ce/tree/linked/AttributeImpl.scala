package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.`type`.Type
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A node in the "linked" tree representing an attribute. Note that this is
 * generated only "on demand", when the attribute is selected by a path expression.<P>
 *
 * <p>It is possible for multiple AttributeImpl objects to represent the same attribute node.
 * The identity of an attribute node is determined by the identity of the element, and the index
 * position of the attribute within the element. Index positions are not reused when an attribute
 * is deleted, and are retained when an attribute is renamed.</p>
 *
 * <p>This object no longer caches information such as the name code and string value, because
 * these would become invalid when the element node is modified.</p>
 *
 * @author Michael H. Kay
 */
class AttributeImpl(element: ElementImpl, index: Int) extends NodeImpl {

  setRawParent(element)

  setSiblingPosition(index)

  /**
   * Get the name of the node
   */
  def getNodeName(): StructuredQName = {
    if (getRawParent == null || getSiblingPosition == -1) {
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
  def isSameNodeInfo(other: NodeInfo): Boolean = {
    if (!(other.isInstanceOf[AttributeImpl])) {
      return false
    }
    if (this == other) {
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
  protected def getSequenceNumber(): Array[Int] = {
    Array(getRawParent.getRawSequenceNumber, 0x8000 + getSiblingPosition)
  }

  /**
   * Return the type of node.
   * @return Node.ATTRIBUTE
   */
  def getNodeKind(): Int = Type.ATTRIBUTE

  /**
   * Return the character value of the node.
   * @return the attribute value
   */
  def getStringValue(): String = {
    getRawParent.asInstanceOf[ElementImpl].getAttributeList
      .getValue(getSiblingPosition)
  }

  /**
   * Get next sibling - not defined for attributes
   */
  def getNextSibling(): NodeInfo = null

  /**
   * Get previous sibling - not defined for attributes
   */
  def getPreviousSibling(): NodeInfo = null

  /**
   * Get the previous node in document order (skipping attributes)
   */
  def getPreviousInDocument(): NodeImpl = getParent.asInstanceOf[NodeImpl]

  /**
   * Get the next node in document order (skipping attributes)
   */
  def getNextInDocument(anchor: NodeImpl): NodeImpl = {
    if (anchor == this) return null
    getParent.asInstanceOf[NodeImpl].getNextInDocument(anchor)
  }

  /**
   * Get sequential key. Returns key of owning element with the attribute index as a suffix
   * @param buffer a buffer to which the generated ID will be written
   */
  def generateId(buffer: FastStringBuffer) {
    getParent.generateId(buffer)
    buffer.append('a')
    buffer.append(Integer toString getSiblingPosition)
  }

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int) {
    val name = getNodeName
    out.attribute(name, getStringValue)
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import ParentNodeImpl._
//remove if not needed
import scala.collection.JavaConversions._

object ParentNodeImpl {

  private val EMPTY_NODE_LIST = new Array[NodeImpl](0)
}

/**
 * ParentNodeImpl is an implementation of a non-leaf node (specifically, an Element node
 * or a Document node)
 * @author Michael H. Kay
 */
abstract class ParentNodeImpl extends NodeImpl {

  private var children: AnyRef = null

  private var sequence: Int = _

  /**
   * Get the node sequence number (in document order). Sequence numbers are monotonic but not
   * consecutive. In the current implementation, parent nodes (elements and document nodes) have a zero
   * least-significant word, while namespaces, attributes, text nodes, comments, and PIs have
   * the top word the same as their owner and the bottom half reflecting their relative position.
   * For nodes added by XQUery Update, the sequence number is -1L
   * @return the sequence number if there is one, or -1L otherwise.
   */
  protected def getSequenceNumber(): Array[Int] = Array(getRawSequenceNumber, 0)

  protected def getRawSequenceNumber(): Int = sequence

  protected def setRawSequenceNumber(seq: Int) {
    sequence = seq
  }

  /**
   * Set the children of this node
   * @param children null if there are no children, a single NodeInfo if there is one child, an array of NodeInfo
   * if there are multiple children
   */
  protected def setChildren(children: AnyRef) {
    this.children = children
  }

  /**
   * Determine if the node has any children.
   */
  def hasChildNodes(): Boolean = (children != null)

  /**
   * Get all children of this node, as an array
   * @return an array containing all the children
   */
  def allChildren(): Array[NodeImpl] = {
    if (children == null) {
      EMPTY_NODE_LIST
    } else if (children.isInstanceOf[NodeImpl]) {
      Array(children.asInstanceOf[NodeImpl])
    } else {
      children.asInstanceOf[Array[NodeImpl]]
    }
  }

  /**
   * Get the first child node of the element
   * @return the first child node of the required type, or null if there are no children
   */
  def getFirstChild(): NodeInfo = {
    if (children == null) return null
    if (children.isInstanceOf[NodeImpl]) return children.asInstanceOf[NodeImpl]
    children.asInstanceOf[Array[NodeImpl]](0)
  }

  /**
   * Get the last child node of the element
   * @return the last child of the element, or null if there are no children
   */
  def getLastChild(): NodeInfo = {
    if (children == null) return null
    if (children.isInstanceOf[NodeImpl]) return children.asInstanceOf[NodeImpl]
    val n = children.asInstanceOf[Array[NodeImpl]]
    n(n.length - 1)
  }

  /**
   * Get the nth child node of the element (numbering from 0)
   * @param n identifies the required child
   * @return the last child of the element, or null if there is no n'th child
   */
  protected def getNthChild(n: Int): NodeImpl = {
    if (children == null) return null
    if (children.isInstanceOf[NodeImpl]) {
      return (if (n == 0) children.asInstanceOf[NodeImpl] else null)
    }
    val nodes = children.asInstanceOf[Array[NodeImpl]]
    if (n < 0 || n >= nodes.length) return null
    nodes(n)
  }

  /**
   * Return the string-value of the node, that is, the concatenation
   * of the character content of all descendent elements and text nodes.
   * @return the accumulated character content of the element, including descendant elements.
   */
  def getStringValue(): String = {
    var sb: FastStringBuffer = null
    var next = getFirstChild.asInstanceOf[NodeImpl]
    while (next != null) {
      if (next.isInstanceOf[TextImpl]) {
        if (sb == null) {
          sb = new FastStringBuffer(FastStringBuffer.SMALL)
        }
        sb.append(next.getStringValue)
      }
      next = next.getNextInDocument(this)
    }
    if (sb == null) return ""
    sb.toString
  }

  /**
   * Add a child node to this node. For system use only. Note: normalizing adjacent text nodes
   * is the responsibility of the caller.
   * @param node the node to be added as a child of this node. This must be an instance of
   * [[client.net.sf.saxon.ce.tree.linked.NodeImpl]]. It will be modified as a result of this call (by setting its
   * parent property and sibling position)
   * @param index the position where the child is to be added
   */
  protected def addChild(node: NodeImpl, index: Int) {
    synchronized {
      var c: Array[NodeImpl] = null
      if (children == null) {
        c = Array.ofDim[NodeImpl](10)
      } else if (children.isInstanceOf[NodeImpl]) {
        c = Array.ofDim[NodeImpl](10)
        c(0) = children.asInstanceOf[NodeImpl]
      } else {
        c = children.asInstanceOf[Array[NodeImpl]]
      }
      if (index >= c.length) {
        val kids = Array.ofDim[NodeImpl](c.length * 2)
        System.arraycopy(c, 0, kids, 0, c.length)
        c = kids
      }
      c(index) = node
      node.setRawParent(this)
      node.setSiblingPosition(index)
      children = c
    }
  }

  /**
   * Compact the space used by this node
   * @param size the number of actual children
   */
  def compact(size: Int) {
    synchronized {
      if (size == 0) {
        children = null
      } else if (size == 1) {
        if (children.isInstanceOf[Array[NodeImpl]]) {
          children = children.asInstanceOf[Array[NodeImpl]](0)
        }
      } else {
        val kids = Array.ofDim[NodeImpl](size)
        System.arraycopy(children, 0, kids, 0, size)
        children = kids
      }
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.om.{NodeInfo, StructuredQName}

object AnyChildNodeTest {

  private val THE_INSTANCE = new AnyChildNodeTest()

  /**
   * Get the singular instance of this class
   * @return the singular instance
   */
  def getInstance(): AnyChildNodeTest = THE_INSTANCE
}

/**
 * An AnyChildNodePattern is the pattern node(), which matches any node except a root node,
 * an attribute node, or a namespace node: in other words, any node that is the child of another
 * node.
 */
class AnyChildNodeTest private () extends NodeTest {

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeKind The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeKind: Int, qName: StructuredQName): Boolean = {
    nodeKind == Type.ELEMENT || nodeKind == Type.TEXT || nodeKind == Type.COMMENT ||
      nodeKind == Type.PROCESSING_INSTRUCTION
  }

  /**
   * Test whether this node test is satisfied by a given node. This alternative
   * method is used in the case of nodes where calculating the fingerprint is expensive,
   * for example DOM or JDOM nodes.
   * @param node the node to be matched
   */
  override def matches(node: NodeInfo): Boolean = {
    val nodeKind = node.getNodeKind
    nodeKind == Type.ELEMENT || nodeKind == Type.TEXT || nodeKind == Type.COMMENT ||
      nodeKind == Type.PROCESSING_INSTRUCTION
  }

  /**
   * Determine the default priority to use if this pattern appears as a match pattern
   * for a template with no explicit priority attribute.
   */
  def getDefaultPriority(): Double = -0.5

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  override def getNodeKindMask(): Int = {
    1 << Type.ELEMENT | 1 << Type.TEXT | 1 << Type.COMMENT | 
      1 << Type.PROCESSING_INSTRUCTION
  }

  override def toString(): String = "node()"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = "AnyChildNodeTest".hashCode
}

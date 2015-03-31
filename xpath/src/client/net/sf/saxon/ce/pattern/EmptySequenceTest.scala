// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.om.{NodeInfo, StructuredQName}

object EmptySequenceTest {

  private val THE_INSTANCE: EmptySequenceTest = new EmptySequenceTest()

  /**
   * Get a NoNodeTest instance
   */
  def getInstance(): EmptySequenceTest = THE_INSTANCE
}

/**
 * NodeTest is an interface that enables a test of whether a node has a particular
 * name and type. An EmptySequenceTest matches no nodes or atomic values: it corresponds to the
 * type empty-sequence().
 *
 * @author Michael H. Kay
 */
class EmptySequenceTest private () extends NodeTest {

  override def getRequiredNodeKind: Int = Type.EMPTY

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeType The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeType: Int, qName: StructuredQName): Boolean = false

  /**
   * Test whether this node test is satisfied by a given node. This alternative
   * method is used in the case of nodes where calculating the fingerprint is expensive,
   * for example DOM or JDOM nodes.
   * @param node the node to be matched
   */
  override def matches(node: NodeInfo): Boolean = false

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority: Double = -0.5

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  override def getNodeKindMask: Int = 0

  override def toString(): String = "empty-sequence()"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = "NoNodeTest".hashCode
}

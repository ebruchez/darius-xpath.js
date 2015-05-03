// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.pattern

import org.orbeon.darius.xpath.`type`.{AnyItemType, ItemType, Type}
import org.orbeon.darius.xpath.om.{NodeInfo, StructuredQName}

object AnyNodeTest {

  private var THE_INSTANCE: AnyNodeTest = new AnyNodeTest()

  /**
   * Get an instance of AnyNodeTest
   */
  def getInstance: AnyNodeTest = THE_INSTANCE
}

/**
 * NodeTest is an interface that enables a test of whether a node has a particular
 * name and type. An AnyNodeTest matches any node.
 *
 * @author Michael H. Kay
 */
class AnyNodeTest private () extends NodeTest {

  override def getSuperType: ItemType = AnyItemType.getInstance

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeType The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeType: Int, qName: StructuredQName): Boolean = true

  /**
   * Test whether this node test is satisfied by a given node. This alternative
   * method is used in the case of nodes where calculating the fingerprint is expensive,
   * for example DOM or JDOM nodes.
   * @param node the node to be matched
   */
  override def matches(node: NodeInfo): Boolean = true

  /**
   * Test whether this QNameTest matches a given QName
   * @param qname the QName to be matched
   * @return true if the name matches, false if not
   */
  def matches(qname: StructuredQName): Boolean = true

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority: Double = -0.5

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  override def getNodeKindMask: Int = {
    1 << Type.ELEMENT | 1 << Type.TEXT | 1 << Type.COMMENT | 
      1 << Type.PROCESSING_INSTRUCTION | 
      1 << Type.ATTRIBUTE | 
      1 << Type.NAMESPACE | 
      1 << Type.DOCUMENT
  }

  override def toString: String = "node()"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = "AnyNodeTest".hashCode
}

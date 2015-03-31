// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.om.{NodeInfo, StructuredQName}

/**
 * NodeTest is an interface that enables a test of whether a node has a particular
 * name and type. A NamespaceTest matches the node type and the namespace URI.
 *
 * @author Michael H. Kay
 */
class NamespaceTest(var nodeKind: Int, var uri: String) extends NodeTest {

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeType The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeType: Int, qName: StructuredQName): Boolean = {
    qName != null && nodeType == nodeKind && uri == qName.getNamespaceURI
  }

  /**
   * Test whether this node test is satisfied by a given node. This alternative
   * method is used in the case of nodes where calculating the fingerprint is expensive,
   * for example DOM or JDOM nodes.
   * @param node the node to be matched
   */
  override def matches(node: NodeInfo): Boolean = {
    node.getNodeKind == nodeKind && node.getURI == uri
  }

  /**
   * Test whether this QNameTest matches a given QName
   * @param qname the QName to be matched
   * @return true if the name matches, false if not
   */
  def matches(qname: StructuredQName): Boolean = qname.getNamespaceURI == uri

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority(): Double = -0.25

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * For patterns that match nodes of several types, return Type.NODE
   * @return the type of node matched by this pattern. e.g. Type.ELEMENT or Type.TEXT
   */
  override def getRequiredNodeKind(): Int = nodeKind

  /**
   * Get the type from which this item type is derived by restriction. This
   * is the supertype in the XPath type heirarchy, as distinct from the Schema
   * base type: this means that the supertype of xs:boolean is xs:anyAtomicType,
   * whose supertype is item() (rather than xs:anySimpleType).
   * <p>
   * In fact the concept of "supertype" is not really well-defined, because the types
   * form a lattice rather than a hierarchy. The only real requirement on this function
   * is that it returns a type that strictly subsumes this type, ideally as narrowly
   * as possible.
   * @return the supertype, or null if this type is item()
   */
  override def getSuperType(): ItemType = NodeKindTest.makeNodeKindTest(nodeKind)

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  override def getNodeKindMask(): Int = 1 << nodeKind

  /**
   * Get the namespace URI matched by this NamespaceTest
   * @return  the namespace URI matched by this NamespaceTest
   */
  def getNamespaceURI: String = uri

  override def toString(): String = '{' + uri + "}:*"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = uri.hashCode ^ nodeKind

  /**
   * Indicates whether some other object is "equal to" this one.
   */
  override def equals(other: Any): Boolean = {
    other.isInstanceOf[NamespaceTest] && 
      other.asInstanceOf[NamespaceTest].nodeKind == nodeKind && 
      other.asInstanceOf[NamespaceTest].uri == uri
  }
}

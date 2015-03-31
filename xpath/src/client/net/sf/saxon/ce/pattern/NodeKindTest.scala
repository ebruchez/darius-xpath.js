// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.om.{NodeInfo, StructuredQName}

object NodeKindTest {

  val DOCUMENT = new NodeKindTest(Type.DOCUMENT)

  val ELEMENT = new NodeKindTest(Type.ELEMENT)

  val ATTRIBUTE = new NodeKindTest(Type.ATTRIBUTE)

  val TEXT = new NodeKindTest(Type.TEXT)

  val COMMENT = new NodeKindTest(Type.COMMENT)

  val PROCESSING_INSTRUCTION = new NodeKindTest(Type.PROCESSING_INSTRUCTION)

  val NAMESPACE = new NodeKindTest(Type.NAMESPACE)

  /**
   * Make a test for a given kind of node
   */
  def makeNodeKindTest(kind: Int): NodeTest = kind match {
    case Type.DOCUMENT ⇒ DOCUMENT
    case Type.ELEMENT ⇒ ELEMENT
    case Type.ATTRIBUTE ⇒ ATTRIBUTE
    case Type.COMMENT ⇒ COMMENT
    case Type.TEXT ⇒ TEXT
    case Type.PROCESSING_INSTRUCTION ⇒ PROCESSING_INSTRUCTION
    case Type.NAMESPACE ⇒ NAMESPACE
    case Type.NODE ⇒ AnyNodeTest.getInstance
    case _ ⇒ throw new IllegalArgumentException("Unknown node kind in NodeKindTest")
  }

  def toString(kind: Int): String = kind match {
    case Type.DOCUMENT ⇒ "document-node()"
    case Type.ELEMENT ⇒ "element()"
    case Type.ATTRIBUTE ⇒ "attribute()"
    case Type.COMMENT ⇒ "comment()"
    case Type.TEXT ⇒ "text()"
    case Type.PROCESSING_INSTRUCTION ⇒ "processing-instruction()"
    case Type.NAMESPACE ⇒ "namespace()"
    case _ ⇒ "** error **"
  }

  /**
   * Get the name of a node kind
   * @param kind the node kind, for example Type.ELEMENT or Type.ATTRIBUTE
   * @return the name of the node kind, for example "element" or "attribute"
   */
  def nodeKindName(kind: Int): String = kind match {
    case Type.DOCUMENT ⇒ "document"
    case Type.ELEMENT ⇒ "element"
    case Type.ATTRIBUTE ⇒ "attribute"
    case Type.COMMENT ⇒ "comment"
    case Type.TEXT ⇒ "text"
    case Type.PROCESSING_INSTRUCTION ⇒ "processing-instruction"
    case Type.NAMESPACE ⇒ "namespace"
    case _ ⇒ "** error **"
  }
}

/**
 * NodeTest is an interface that enables a test of whether a node has a particular
 * name and kind. A NodeKindTest matches the node kind only.
 *
 * @author Michael H. Kay
 */
class NodeKindTest private (var kind: Int) extends NodeTest {

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeKind The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeKind: Int, qName: StructuredQName): Boolean = kind == nodeKind

  /**
   * Test whether this node test is satisfied by a given node. This alternative
   * method is used in the case of nodes where calculating the fingerprint is expensive,
   * for example DOM or JDOM nodes.
   * @param node the node to be matched
   */
  override def matches(node: NodeInfo): Boolean = node.getNodeKind == kind

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority: Double = -0.5

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * @return the type of node matched by this pattern. e.g. Type.ELEMENT or Type.TEXT
   */
  override def getRequiredNodeKind: Int = kind

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  override def getNodeKindMask: Int = 1 << kind

  /**
   * Get the content type allowed by this NodeTest (that is, the type annotation).
   * Return AnyType if there are no restrictions. The default implementation returns AnyType.
   */
  override def getAtomizedItemType: AtomicType = kind match {
    case Type.DOCUMENT ⇒ AtomicType.UNTYPED_ATOMIC
    case Type.ELEMENT ⇒ AtomicType.ANY_ATOMIC
    case Type.ATTRIBUTE ⇒ AtomicType.ANY_ATOMIC
    case Type.COMMENT ⇒ AtomicType.STRING
    case Type.TEXT ⇒ AtomicType.UNTYPED_ATOMIC
    case Type.PROCESSING_INSTRUCTION ⇒ AtomicType.STRING
    case Type.NAMESPACE ⇒ AtomicType.STRING
    case _ ⇒ throw new AssertionError("Unknown node kind")
  }

  override def toString(): String = NodeKindTest.toString(kind)

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = kind

  /**
   * Indicates whether some other object is "equal to" this one.
   */
  override def equals(other: Any): Boolean = other match {
    case other: NodeKindTest ⇒ other.kind == kind
    case _ ⇒ false
  }
}

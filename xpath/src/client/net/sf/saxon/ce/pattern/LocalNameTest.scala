package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * NodeTest is an interface that enables a test of whether a node has a particular
 * name and type. A LocalNameTest matches the node type and the local name,
 * it represents an XPath 2.0 test of the form *:name.
 *
 * @author Michael H. Kay
 */
class LocalNameTest(var nodeKind: Int, @BeanProperty var localName: String) extends NodeTest {

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeType The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeType: Int, qName: StructuredQName): Boolean = {
    if (qName == null) return false
    if (nodeType != nodeKind) return false
    localName == qName.getLocalName
  }

  /**
   * Test whether this node test is satisfied by a given node. This alternative
   * method is used in the case of nodes where calculating the fingerprint is expensive,
   * for example DOM or JDOM nodes.
   * @param node the node to be matched
   */
  def matches(node: NodeInfo): Boolean = {
    localName == node.getLocalPart && nodeKind == node.getNodeKind
  }

  /**
   * Test whether this QNameTest matches a given QName
   * @param qname the QName to be matched
   * @return true if the name matches, false if not
   */
  def matches(qname: StructuredQName): Boolean = localName == qname.getLocalName

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority(): Double = -0.25

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * For patterns that match nodes of several types, return Type.NODE
   * @return the type of node matched by this pattern. e.g. Type.ELEMENT or Type.TEXT
   */
  def getRequiredNodeKind(): Int = nodeKind

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  def getNodeKindMask(): Int = 1 << nodeKind

  override def toString(): String = "*:" + localName

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = nodeKind << 20 ^ localName.hashCode

  /**
   * Indicates whether some other object is "equal to" this one.
   */
  override def equals(other: Any): Boolean = {
    other.isInstanceOf[LocalNameTest] && 
      other.asInstanceOf[LocalNameTest].nodeKind == nodeKind && 
      other.asInstanceOf[LocalNameTest].localName == localName
  }
}

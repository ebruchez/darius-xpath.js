package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import AnyNodeTest._
//remove if not needed
import scala.collection.JavaConversions._

object AnyNodeTest {

  private var THE_INSTANCE: AnyNodeTest = new AnyNodeTest()

  /**
   * Get an instance of AnyNodeTest
   */
  def getInstance(): AnyNodeTest = THE_INSTANCE
}

/**
 * NodeTest is an interface that enables a test of whether a node has a particular
 * name and type. An AnyNodeTest matches any node.
 *
 * @author Michael H. Kay
 */
class AnyNodeTest private () extends NodeTest {

  def getSuperType(): ItemType = AnyItemType.getInstance

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
  def matches(node: NodeInfo): Boolean = true

  /**
   * Test whether this QNameTest matches a given QName
   * @param qname the QName to be matched
   * @return true if the name matches, false if not
   */
  def matches(qname: StructuredQName): Boolean = true

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority(): Double = -0.5

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  def getNodeKindMask(): Int = {
    1 << Type.ELEMENT | 1 << Type.TEXT | 1 << Type.COMMENT | 
      1 << Type.PROCESSING_INSTRUCTION | 
      1 << Type.ATTRIBUTE | 
      1 << Type.NAMESPACE | 
      1 << Type.DOCUMENT
  }

  override def toString(): String = "node()"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = "AnyNodeTest".hashCode
}

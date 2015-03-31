// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.om.{Axis, NodeInfo, StructuredQName}

import scala.beans.BeanProperty

class DocumentNodeTest(@BeanProperty var elementTest: NodeTest) extends NodeTest {

  /**
   * Test whether this node test is satisfied by a given node
   * @param nodeKind The type of node to be matched
   * @param qName identifies the expanded name of the node to be matched
   */
  def matches(nodeKind: Int, qName: StructuredQName): Boolean = {
    throw new UnsupportedOperationException("DocumentNodeTest doesn't support this method")
  }

  /**
   * Determine whether this Pattern matches the given Node.
   * @param node The NodeInfo representing the Element or other node to be tested against the Pattern
   * uses variables, or contains calls on functions such as document() or key().
   * @return true if the node matches the Pattern, false otherwise
   */
  override def matches(node: NodeInfo): Boolean = {
    if (node.getNodeKind != Type.DOCUMENT) {
      return false
    }
    val iter = node.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
    var found = false
    while (true) {
      val n = iter.next().asInstanceOf[NodeInfo]
      if (n == null) {
        return found
      }
      val kind = n.getNodeKind
      if (kind == Type.TEXT) {
        false
      } else if (kind == Type.ELEMENT) {
        if (found) {
          return false
        }
        if (elementTest.matches(n)) {
          found = true
        } else {
          return false
        }
      }
    }
    throw new IllegalStateException
  }

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  def getDefaultPriority: Double = elementTest.getDefaultPriority

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * @return the type of node matched by this pattern. e.g. Type.ELEMENT or Type.TEXT
   */
  override def getRequiredNodeKind: Int = Type.DOCUMENT

  /**
   * Get a mask indicating which kinds of nodes this NodeTest can match. This is a combination
   * of bits: 1<<Type.ELEMENT for element nodes, 1<<Type.TEXT for text nodes, and so on.
   */
  override def getNodeKindMask: Int = 1 << Type.DOCUMENT

  override def toString(): String = {
    "document-node(" + elementTest.toString + ')'
  }

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = elementTest.hashCode ^ 12345

  override def equals(other: Any): Boolean = other match {
    case other: DocumentNodeTest ⇒ other.elementTest == elementTest
    case _ ⇒ false
  }
}

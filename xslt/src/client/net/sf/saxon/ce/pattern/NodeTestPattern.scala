// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.NodeInfo

import scala.beans.BeanProperty

/**
 * A NodeTestPattern is a pattern that consists simply of a NodeTest.
 * @author Michael H. Kay
 */
class NodeTestPattern extends Pattern {

  @BeanProperty
  var nodeTest: NodeTest = _

  def this(test: NodeTest) {
    this()
    nodeTest = test
  }

  /**
   * Determine whether this Pattern matches the given Node. This is the main external interface
   * for matching patterns: it sets current() to the node being tested
   *
   * @param node The NodeInfo representing the Element or other node to be tested against the Pattern
   * @param context The context in which the match is to take place. Only relevant if the pattern
   * uses variables, or contains calls on functions such as document() or key(). Not used (and can be
   * set to null) in the case of patterns that are NodeTests
   * @return true if the node matches the Pattern, false otherwise
   */
  def matches(node: NodeInfo, context: XPathContext): Boolean = nodeTest.matches(node)

  /**
   * Determine the default priority of this node test when used on its own as a Pattern
   */
  override def getDefaultPriority(): Double = nodeTest.getDefaultPriority

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * For patterns that match nodes of several types, return Type.NODE
   *
   * @return the type of node matched by this pattern. e.g. Type.ELEMENT or Type.TEXT
   */
  override def getNodeKind(): Int = nodeTest.getRequiredNodeKind

  /**
   * Display the pattern for diagnostics
   */
  override def toString: String = nodeTest.toString

  /**
   * Determine whether this pattern is the same as another pattern
   * @param other the other object
   */
  override def equals(other: Any): Boolean = {
    other.isInstanceOf[NodeTestPattern] &&
      other.asInstanceOf[NodeTestPattern].nodeTest == nodeTest
  }

  /**
   * Hashcode supporting equals()
   */
  override def hashCode(): Int = 0x7aeffea8 ^ nodeTest.hashCode
}

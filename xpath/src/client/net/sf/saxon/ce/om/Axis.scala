// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.trans.XPathException

object Axis {

  /**
   * Constant representing the ancestor axis
   */
  val ANCESTOR: Byte = 0

  /**
   Constant representing the ancestor-or-self axis
   */
  val ANCESTOR_OR_SELF: Byte = 1

  /**
   Constant representing the attribute axis
   */
  val ATTRIBUTE: Byte = 2

  /**
   Constant representing the child axis
   */
  val CHILD: Byte = 3

  /**
   Constant representing the descendant axis
   */
  val DESCENDANT: Byte = 4

  /**
   Constant representing the descendant-or-self axis
   */
  val DESCENDANT_OR_SELF: Byte = 5

  /**
   Constant representing the following axis
   */
  val FOLLOWING: Byte = 6

  /**
   Constant representing the following-sibling axis
   */
  val FOLLOWING_SIBLING: Byte = 7

  /**
   Constant representing the namespace axis
   */
  val NAMESPACE: Byte = 8

  /**
   Constant representing the parent axis
   */
  val PARENT: Byte = 9

  /**
   Constant representing the preceding axis
   */
  val PRECEDING: Byte = 10

  /**
   Constant representing the preceding-sibling axis
   */
  val PRECEDING_SIBLING: Byte = 11

  /**
   Constant representing the self axis
   */
  val SELF: Byte = 12

  /**
   * Table indicating the principal node type of each axis
   */
  val principalNodeType = Array(Type.ELEMENT, Type.ELEMENT, Type.ATTRIBUTE, Type.ELEMENT, Type.ELEMENT, Type.ELEMENT, Type.ELEMENT, Type.ELEMENT, Type.NAMESPACE, Type.ELEMENT, Type.ELEMENT, Type.ELEMENT, Type.ELEMENT)

  /**
   * Table indicating for each axis whether it is in forwards document order
   */
  val isForwards = Array(false, false, true, true, true, true, true, true, true, true, false, false, true)

  /**
   * Table indicating for each axis whether it is a peer axis. An axis is a peer
   * axis if no node on the axis is an ancestor of another node on the axis.
   */
  val isPeerAxis = Array(false, false, true, true, false, false, false, true, true, true, false, true, true)

  /**
   * Table indicating for each axis whether it is contained within the subtree
   * rooted at the origin node.
   */
  val isSubtreeAxis = Array(false, false, true, true, true, true, false, false, true, false, false, false, true)

  /**
   * Table giving the name of each axis as used in XPath, for example "ancestor-or-self"
   */
  val axisName = Array("ancestor", "ancestor-or-self", "attribute", "child", "descendant", "descendant-or-self", "following", "following-sibling", "namespace", "parent", "preceding", "preceding-sibling", "self")

  /**
   * Resolve an axis name into a symbolic constant representing the axis
   *
   * @param name the axis name
   * @throws XPathException if the axis name is unrecognized
   * @return integer value representing the named axis
   */
  def getAxisNumber(name: String): Byte = {
    for (i ← 0 until 13 if axisName(i) == name) {
      return i.toByte
    }
    throw new XPathException("Unknown axis name: " + name)
  }

  /**
   * The following table indicates the combinations of axis and node-kind that always
   * return an empty result.
   */
  private val DOC = 1 << Type.DOCUMENT

  private val ELE = 1 << Type.ELEMENT

  private val ATT = 1 << Type.ATTRIBUTE

  private val TEX = 1 << Type.TEXT

  private val PIN = 1 << Type.PROCESSING_INSTRUCTION

  private val COM = 1 << Type.COMMENT

  private val NAM = 1 << Type.NAMESPACE

  private val voidAxisTable: Array[Int] = Array(DOC, 0, DOC | ATT | TEX | PIN | COM | NAM, ATT | TEX | PIN | COM | NAM, ATT | TEX | PIN | COM | NAM, 0, DOC, DOC | ATT | NAM, DOC | ATT | TEX | PIN | COM | NAM, DOC, DOC, DOC | ATT | NAM, 0)

  /**
   * Ask whether a given axis can contain any nodes when starting at the specified node kind.
   * For example, the attribute axis when starting at an attribute node will always be empty
   * @param axis the axis, for example [[Axis.ATTRIBUTE]]
   * @param nodeKind the node kind of the origin node, for example [[Type.ATTRIBUTE]]
   * @return true if no nodes will ever appear on the specified axis when starting at the specified
   * node kind.
   */
  def isAlwaysEmpty(axis: Int, nodeKind: Int): Boolean = {
    (voidAxisTable(axis) & (1 << nodeKind)) != 0
  }

  /**
   * The following table indicates the kinds of node found on each axis
   */
  private val nodeKindTable: Array[Int] = Array(DOC | ELE, DOC | ELE | ATT | TEX | PIN | COM | NAM, ATT, ELE | TEX | PIN | COM, ELE | TEX | PIN | COM, DOC | ELE | ATT | TEX | PIN | COM | NAM, ELE | TEX | PIN | COM, ELE | TEX | PIN | COM, NAM, DOC | ELE, DOC | ELE | TEX | PIN | COM, ELE | TEX | PIN | COM, DOC | ELE | ATT | TEX | PIN | COM | NAM)

  /**
   * Determine whether a given kind of node can be found on a given axis. For example,
   * the attribute axis will never contain any element nodes.
   * @param axis the axis, for example [[Axis.ATTRIBUTE]]
   * @param nodeKind the node kind of the origin node, for example [[Type.ELEMENT]]
   * @return true if the given kind of node can appear on the specified axis
   */
  def containsNodeKind(axis: Int, nodeKind: Int): Boolean = {
    (nodeKindTable(axis) & (1 << nodeKind)) != 0
  }

  /**
   * For each axis, determine the inverse axis, in the sense that if A is on axis X starting at B,
   * the B is on the axis inverseAxis[X] starting at A. This doesn't quite work for the PARENT axis,
   * which has no simple inverse: this table gives the inverse as CHILD
   */
  var inverseAxis: Array[Byte] = Array(DESCENDANT, DESCENDANT_OR_SELF, PARENT, PARENT, ANCESTOR, ANCESTOR_OR_SELF, PRECEDING, PRECEDING_SIBLING, PARENT, CHILD, FOLLOWING, FOLLOWING_SIBLING, SELF)
}

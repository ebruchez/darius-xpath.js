// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.`type`

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.pattern.AnyNodeTest
import org.orbeon.darius.xpath.pattern.EmptySequenceTest
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.value.AtomicValue
//remove if not needed
import scala.collection.JavaConversions._

object Type {

  /**
   * Type representing an element node - element()
   */
  val ELEMENT = 1

  /**
   * Item type representing an attribute node - attribute()
   */
  val ATTRIBUTE = 2

  /**
   * Item type representing a text node - text()
   */
  val TEXT = 3

  /**
   * Item type representing a text node stored in the tiny tree as compressed whitespace
   */
  val WHITESPACE_TEXT = 4

  /**
   * Item type representing a processing-instruction node
   */
  val PROCESSING_INSTRUCTION = 7

  /**
   * Item type representing a comment node
   */
  val COMMENT = 8

  /**
   * Item type representing a document node
   */
  val DOCUMENT = 9

  /**
   * Item type representing a doctype node
   */
  val DOCUMENT_TYPE = 10

  /**
   * Item type representing a namespace node
   */
  val NAMESPACE = 13

  /**
   * Dummy node kind used in the tiny tree to mark the end of the tree
   */
  val STOPPER = 11

  /**
   * Dummy node kind used in the tiny tree to contain a parent pointer
   */
  val PARENT_POINTER = 12

  /**
   * An item type that matches any node
   */
  val NODE = 0

  val NODE_TYPE = AnyNodeTest.getInstance

  /**
   * An item type that matches any item
   */
  val ITEM = 88

  val ITEM_TYPE = AnyItemType.getInstance

  /**
   * A type number for function()
   */
  val FUNCTION = 99

  val MAX_NODE_TYPE = 13

  /**
   * Item type that matches no items (corresponds to SequenceType empty())
   */
  val EMPTY = 15

  /**
   * Get the ItemType of an Item
   *
   * @param item the item whose type is required
   * @return the item type of the item
   */
  def getItemType(item: Item): ItemType = {
    if (item.isInstanceOf[AtomicValue]) {
      item.asInstanceOf[AtomicValue].getItemType
    } else if (item.isInstanceOf[NodeInfo]) {
      NodeKindTest.makeNodeKindTest(item.asInstanceOf[NodeInfo].getNodeKind)
    } else {
      null
    }
  }

  /**
   * Output (for diagnostics) a representation of the type of an item. This
   * does not have to be the most specific type
   * @param item the item whose type is to be displayed
   * @return a string representation of the type of the item
   */
  def displayTypeName(item: Item): String = {
    if (item.isInstanceOf[NodeInfo]) {
      val node = item.asInstanceOf[NodeInfo]
      node.getNodeKind match {
        case DOCUMENT ⇒ "document-node()"
        case ELEMENT ⇒ "element(" + item.asInstanceOf[NodeInfo].getDisplayName +
          ')'
        case ATTRIBUTE ⇒ "attribute(" + item.asInstanceOf[NodeInfo].getDisplayName +
          ')'
        case TEXT ⇒ "text()"
        case COMMENT ⇒ "comment()"
        case PROCESSING_INSTRUCTION ⇒ "processing-instruction()"
        case NAMESPACE ⇒ "namespace()"
        case _ ⇒ ""
      }
    } else {
      item.asInstanceOf[AtomicValue].getItemType.toString
    }
  }

  /**
   * Get a type that is a common supertype of two given item types
   *
   *
   * @param t1 the first item type
   * @param t2 the second item type
   * @return the item type that is a supertype of both
   *     the supplied item types
   */
  def getCommonSuperType(t1: ItemType, t2: ItemType): ItemType = {
    if (t1.isInstanceOf[EmptySequenceTest]) {
      return t2
    }
    if (t2.isInstanceOf[EmptySequenceTest]) {
      return t1
    }
    val th = TypeHierarchy.getInstance
    val r = th.relationship(t1, t2)
    if (r == TypeHierarchy.SAME_TYPE) {
      t1
    } else if (r == TypeHierarchy.SUBSUMED_BY) {
      t2
    } else if (r == TypeHierarchy.SUBSUMES) {
      t1
    } else {
      getCommonSuperType(t2.getSuperType, t1)
    }
  }

  /**
   * Determine whether two primitive atomic types are comparable under the rules for ValueComparisons
   * (that is, untyped atomic values treated as strings)
   * @param t1 the first type to compared.
   * This must be a primitive atomic type
   * @param t2 the second type to compared.
   * This must be a primitive atomic type
   * @param ordered true if testing for an ordering comparison (lt, gt, le, ge). False
   * if testing for an equality comparison (eq, ne)
   * @return true if the types are comparable, as defined by the rules of the "eq" operator
   */
  def isComparable(t1: AtomicType, t2: AtomicType, ordered: Boolean): Boolean = {
    if (t1 == t2) {
      return true
    }
    if (t1 == AtomicType.ANY_ATOMIC || t2 == AtomicType.ANY_ATOMIC) {
      return true
    }
    if (t1 == AtomicType.UNTYPED_ATOMIC) {
      t1 = AtomicType.STRING
    }
    if (t2 == AtomicType.UNTYPED_ATOMIC) {
      t2 = AtomicType.STRING
    }
    if (t1 == AtomicType.ANY_URI) {
      t1 = AtomicType.STRING
    }
    if (t2 == AtomicType.ANY_URI) {
      t2 = AtomicType.STRING
    }
    if (t1.isPrimitiveNumeric) {
      t1 = AtomicType.NUMERIC
    }
    if (t2.isPrimitiveNumeric) {
      t2 = AtomicType.NUMERIC
    }
    if (!ordered) {
      if (t1 == AtomicType.DAY_TIME_DURATION) {
        t1 = AtomicType.DURATION
      }
      if (t2 == AtomicType.DAY_TIME_DURATION) {
        t2 = AtomicType.DURATION
      }
      if (t1 == AtomicType.YEAR_MONTH_DURATION) {
        t1 = AtomicType.DURATION
      }
      if (t2 == AtomicType.YEAR_MONTH_DURATION) {
        t2 = AtomicType.DURATION
      }
    }
    t1 == t2
  }
}

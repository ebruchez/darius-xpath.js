// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.`type`

import client.net.sf.saxon.ce.om.{Item, NodeInfo}
import client.net.sf.saxon.ce.pattern.{AnyNodeTest, EmptySequenceTest, NodeKindTest}
import client.net.sf.saxon.ce.value.AtomicValue

object Type {

  /**
   * Type representing an element node - element()
   */
  val ELEMENT: Short = 1

  /**
   * Item type representing an attribute node - attribute()
   */
  val ATTRIBUTE: Short = 2

  /**
   * Item type representing a text node - text()
   */
  val TEXT: Short = 3

  /**
   * Item type representing a text node stored in the tiny tree as compressed whitespace
   */
  val WHITESPACE_TEXT: Short = 4

  /**
   * Item type representing a processing-instruction node
   */
  val PROCESSING_INSTRUCTION: Short = 7

  /**
   * Item type representing a comment node
   */
  val COMMENT: Short = 8

  /**
   * Item type representing a document node
   */
  val DOCUMENT: Short = 9

  /**
   * Item type representing a doctype node
   */
  val DOCUMENT_TYPE: Short = 10

  /**
   * Item type representing a namespace node
   */
  val NAMESPACE: Short = 13

  /**
   * Dummy node kind used in the tiny tree to mark the end of the tree
   */
  val STOPPER: Short = 11

  /**
   * Dummy node kind used in the tiny tree to contain a parent pointer
   */
  val PARENT_POINTER: Short = 12

  /**
   * An item type that matches any node
   */
  val NODE: Short = 0

  val NODE_TYPE = AnyNodeTest.getInstance

  /**
   * An item type that matches any item
   */
  val ITEM: Short = 88

  val ITEM_TYPE = AnyItemType.getInstance

  /**
   * A type number for function()
   */
  val FUNCTION: Short = 99

  val MAX_NODE_TYPE: Short = 13

  /**
   * Item type that matches no items (corresponds to SequenceType empty())
   */
  val EMPTY: Short = 15

  /**
   * Get the ItemType of an Item
   *
   * @param item the item whose type is required
   * @return the item type of the item
   */
  def getItemType(item: Item): ItemType = {
    item match {
      case value: AtomicValue ⇒
        value.getItemType
      case info: NodeInfo ⇒
        NodeKindTest.makeNodeKindTest(info.getNodeKind)
      case _ ⇒
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
    item match {
      case node: NodeInfo ⇒
        node.getNodeKind match {
          case DOCUMENT ⇒ "document-node()"
          case ELEMENT ⇒ "element(" + node.getDisplayName +
            ')'
          case ATTRIBUTE ⇒ "attribute(" + node.getDisplayName +
            ')'
          case TEXT ⇒ "text()"
          case COMMENT ⇒ "comment()"
          case PROCESSING_INSTRUCTION ⇒ "processing-instruction()"
          case NAMESPACE ⇒ "namespace()"
          case _ ⇒ ""
        }
      case _ ⇒
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
    var _t1 = t1
    var _t2 = t2
    if (_t1 == _t2) {
      return true
    }
    if (_t1 == AtomicType.ANY_ATOMIC || _t2 == AtomicType.ANY_ATOMIC) {
      return true
    }
    if (_t1 == AtomicType.UNTYPED_ATOMIC) {
      _t1 = AtomicType.STRING
    }
    if (_t2 == AtomicType.UNTYPED_ATOMIC) {
      _t2 = AtomicType.STRING
    }
    if (_t1 == AtomicType.ANY_URI) {
      _t1 = AtomicType.STRING
    }
    if (_t2 == AtomicType.ANY_URI) {
      _t2 = AtomicType.STRING
    }
    if (_t1.isPrimitiveNumeric) {
      _t1 = AtomicType.NUMERIC
    }
    if (_t2.isPrimitiveNumeric) {
      _t2 = AtomicType.NUMERIC
    }
    if (!ordered) {
      if (_t1 == AtomicType.DAY_TIME_DURATION) {
        _t1 = AtomicType.DURATION
      }
      if (_t2 == AtomicType.DAY_TIME_DURATION) {
        _t2 = AtomicType.DURATION
      }
      if (_t1 == AtomicType.YEAR_MONTH_DURATION) {
        _t1 = AtomicType.DURATION
      }
      if (_t2 == AtomicType.YEAR_MONTH_DURATION) {
        _t2 = AtomicType.DURATION
      }
    }
    _t1 == _t2
  }
}

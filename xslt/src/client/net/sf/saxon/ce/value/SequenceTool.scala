// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import org.orbeon.darius.xpath.event.SequenceReceiver
import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.js.JSObjectType
import org.orbeon.darius.xpath.js.JSObjectValue
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.pattern._
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.UnfailingIterator
import org.orbeon.darius.xpath.`type`.AnyItemType
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.`type`.Type
//remove if not needed
import scala.collection.JavaConversions._

object SequenceTool {

  /**
   * Static method to make an Item from a Value
   * @param value the value to be converted
   * @return null if the value is an empty sequence; or the only item in the value
   * if it is a singleton sequence
   * @throws XPathException if the Value contains multiple items
   */
  def asItem(value: Sequence): Item = {
    if (value.isInstanceOf[Item]) {
      value.asInstanceOf[Item]
    } else {
      val iter = value.iterate()
      val item = iter.next()
      if (item == null) {
        null
      } else if (iter.next() != null) {
        throw new XPathException("Attempting to access a sequence as a singleton item")
      } else {
        item
      }
    }
  }

  /**
   * Determine the data type of the items in the sequence
   * @return for the default implementation: AnyItemType (not known)
   */
  def getItemTypeOfValue(`val`: Sequence): ItemType = {
    val iter = `val`.iterate()
    var `type`: ItemType = null
    var item = iter.next()
    if (item == null) {
      return EmptySequenceTest.getInstance
    } else {
      `type` = getItemType(item)
    }
    while (true) {
      if (`type` == AnyItemType.getInstance) {
        return `type`
      }
      item = iter.next()
      if (item == null) {
        //break
      }
      `type` = Type.getCommonSuperType(`type`, getItemType(item))
    }
    `type`
  }

  def getItemType(item: Item): ItemType = {
    if (item.isInstanceOf[NodeInfo]) {
      val node = item.asInstanceOf[NodeInfo]
      node.getNodeKind match {
        case Type.DOCUMENT ⇒
          var iter = node.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
          var elementType: ItemType = null
          while (true) {
            val n = iter.next().asInstanceOf[NodeInfo]
            if (n == null) {
              //break
            }
            val kind = n.getNodeKind
            if (kind == Type.TEXT) {
              elementType = null
              //break
            } else if (kind == Type.ELEMENT) {
              if (elementType != null) {
                elementType = null
                //break
              }
              elementType = getItemType(n)
            }
          }
          if (elementType == null) {
            NodeKindTest.DOCUMENT
          } else {
            new DocumentNodeTest(elementType.asInstanceOf[NodeTest])
          }

        case Type.ELEMENT ⇒ new NameTest(Type.ELEMENT, node.getNodeName)
        case Type.ATTRIBUTE ⇒ new NameTest(Type.ATTRIBUTE, node.getNodeName)
        case Type.TEXT ⇒ NodeKindTest.TEXT
        case Type.COMMENT ⇒ NodeKindTest.COMMENT
        case Type.PROCESSING_INSTRUCTION ⇒ NodeKindTest.PROCESSING_INSTRUCTION
        case Type.NAMESPACE ⇒ NodeKindTest.NAMESPACE
        case _ ⇒ throw new IllegalArgumentException("Unknown node kind " + node.getNodeKind)
      }
    } else if (item.isInstanceOf[JSObjectValue]) {
      new JSObjectType()
    } else {
      item.asInstanceOf[AtomicValue].getItemType
    }
  }

  /**
   * Process a value in push mode, without returning any tail calls
   * @param iterator iterator over the value to be pushed
   * @param context The dynamic context, giving access to the current node,
   * the current variables, etc.
   */
  def process(iterator: SequenceIterator, context: XPathContext): Unit = {
    val out = context.getReceiver
    while (true) {
      val it = iterator.next()
      if (it == null) //break
      out.append(it, NodeInfo.ALL_NAMESPACES)
    }
  }
}

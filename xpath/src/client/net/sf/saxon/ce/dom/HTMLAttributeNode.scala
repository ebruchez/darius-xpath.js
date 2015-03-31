// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.dom

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.{AnyNodeTest, NodeTest}
import client.net.sf.saxon.ce.tree.iter._
import client.net.sf.saxon.ce.tree.util.{FastStringBuffer, Navigator}
import client.net.sf.saxon.ce.value.{AbstractNode, AtomicValue, UntypedAtomicValue}

/**
 * An attribute node in the DOM - may be XML or HTML
 */
class HTMLAttributeNode(
  var element : HTMLNodeWrapper,
  var name    : String,
  var prefix  : String,
  var uri     : String,
  var value   : String
) extends AbstractNode
  with NodeInfo {

  private val qname = new StructuredQName(prefix, uri, name)

  def getNodeKind: Int = Type.ATTRIBUTE

  def isSameNodeInfo(other: NodeInfo): Boolean = {
    other.isInstanceOf[HTMLAttributeNode] &&
      element.isSameNodeInfo(other.asInstanceOf[HTMLAttributeNode].element) &&
      name == other.asInstanceOf[HTMLAttributeNode].name
  }

  def getSystemId: String = element.getSystemId

  def getBaseURI: String = element.getBaseURI

  def compareOrder(other: NodeInfo): Int = {
    if (other.isInstanceOf[HTMLAttributeNode]) {
      if (element.isSameNodeInfo(other.asInstanceOf[HTMLAttributeNode].element)) {
        return qname.compareTo(other.asInstanceOf[HTMLAttributeNode].qname)
      } else {
        return element.compareOrder(other.asInstanceOf[HTMLAttributeNode].element)
      }
    }
    if (other.isSameNodeInfo(element)) {
      return +1
    }
    element.compareOrder(other)
  }

  def getStringValue: String = value

  def getNodeName: StructuredQName = qname

  def getLocalPart: String = name

  def getURI: String = uri

  def getDisplayName: String = {
    if (prefix.length == 0) name else prefix + ':' + name
  }

  def getParent: NodeInfo = element

  private def iterateAxis0(axisNumber: Byte): UnfailingIterator = axisNumber match {
    case Axis.ANCESTOR ⇒ element.iterateAxis(Axis.ANCESTOR_OR_SELF, AnyNodeTest.getInstance)
    case Axis.ANCESTOR_OR_SELF ⇒ new PrependIterator(this, element.iterateAxis(Axis.ANCESTOR_OR_SELF, AnyNodeTest.getInstance))
    case Axis.ATTRIBUTE ⇒ EmptyIterator.getInstance
    case Axis.CHILD ⇒ EmptyIterator.getInstance
    case Axis.DESCENDANT ⇒ EmptyIterator.getInstance
    case Axis.DESCENDANT_OR_SELF ⇒ SingletonIterator.makeIterator(this)
    case Axis.FOLLOWING ⇒ new Navigator.FollowingEnumeration(this)
    case Axis.FOLLOWING_SIBLING ⇒ EmptyIterator.getInstance
    case Axis.NAMESPACE ⇒ EmptyIterator.getInstance
    case Axis.PARENT ⇒ SingletonIterator.makeIterator(element)
    case Axis.PRECEDING ⇒ new Navigator.PrecedingEnumeration(this, false)
    case Axis.PRECEDING_SIBLING ⇒ EmptyIterator.getInstance
    case Axis.SELF ⇒ SingletonIterator.makeIterator(this)
    case _ ⇒ throw new IllegalArgumentException("Unknown axis number " + axisNumber)
  }

  def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = {
    Navigator.newAxisFilter(iterateAxis0(axisNumber), nodeTest)
  }

  def getRoot: NodeInfo = element.getRoot

  def getDocumentRoot: DocumentInfo = element.getDocumentRoot

  def hasChildNodes: Boolean = false

  def generateId(buffer: FastStringBuffer): Unit = {
    element.generateId(buffer)
    buffer.append("" + name.hashCode)
  }

  def getDocumentNumber: Int = element.getDocumentNumber

  def copy(out: Receiver, copyOptions: Int): Unit = {
    out.attribute(qname, value)
  }

  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = NamespaceBinding.EMPTY_ARRAY

  def getTypedValue: AtomicValue = new UntypedAtomicValue(value)

  def getSiblingPosition: Int = 1
}

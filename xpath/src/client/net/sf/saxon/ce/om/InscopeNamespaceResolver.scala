// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.orbeon.Iterator
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.tree.util.NamespaceIterator

import scala.beans.BeanProperty
import scala.util.control.Breaks

/**
 * A NamespaceResolver that resolves namespace prefixes by reference to a node in a document for which
 * those namespaces are in-scope.
 */
class InscopeNamespaceResolver(_node: NodeInfo) extends NamespaceResolver {

  @BeanProperty
  var node: NodeInfo = if (_node.getNodeKind == Type.ELEMENT) _node else _node.getParent

  /**
   * Get the namespace URI corresponding to a given prefix. Return null
   * if the prefix is not in scope.
   *
   * @param prefix     the namespace prefix
   * @param useDefault true if the default namespace is to be used when the
   *                   prefix is ""
   * @return the uri for the namespace, or null if the prefix is not in scope
   * Return "" for the no-namespace.
   */
  def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    if ("" == prefix && !useDefault) {
      return ""
    }
    val iter = node.iterateAxis(Axis.NAMESPACE, AnyNodeTest.getInstance)
    import Breaks._
    breakable {
      while (true) {
        val node = iter.next().asInstanceOf[NodeInfo]
        if (node == null) {
          break()
        }
        if (node.getLocalPart == prefix) {
          return node.getStringValue
        }
      }
    }
    if ("" == prefix) {
      ""
    } else {
      null
    }
  }

  /**
   * Get an iterator over all the prefixes declared in this namespace context. This will include
   * the default namespace (prefix="") and the XML namespace where appropriate
   */
  def iteratePrefixes(): Iterator[String] = {
    new Iterator[String]() {

      var phase: Int = 0

      var iter: Iterator[NamespaceBinding] = NamespaceIterator.iterateNamespaces(node)

      def hasNext: Boolean = {
        if (iter.hasNext) {
          true
        } else if (phase == 0) {
          phase = 1
          true
        } else {
          false
        }
      }

      def next(): String = {
        if (phase == 1) {
          phase = 2
          "xml"
        } else {
          iter.next().getPrefix
        }
      }

      def remove(): Unit = {
        throw new UnsupportedOperationException("remove")
      }
    }
  }
}

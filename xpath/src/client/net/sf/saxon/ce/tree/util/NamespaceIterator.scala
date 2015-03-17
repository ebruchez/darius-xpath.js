package client.net.sf.saxon.ce.tree.util

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import java.util.Collections
import java.util.HashSet
import java.util.Iterator
import NamespaceIterator._
//remove if not needed
import scala.collection.JavaConversions._

object NamespaceIterator {

  /**
   * Factory method: create an iterator over the in-scope namespace bindings for an element
   * @param element the element (or other node) whose in-scope namespaces are required. If this
   * is not an element, the result will be an empty iterator
   * @return an iterator over the namespace bindings. A namespace binding represents
   * a prefix-uri binding; the prefix and URI can be obtained by reference to the name pool. This
   * iterator will represent all the in-scope namespaces, without duplicates, and respecting namespace
   * undeclarations. It does not include the XML namespace.
   */
  def iterateNamespaces(element: NodeInfo): Iterator[NamespaceBinding] = {
    if (element.getNodeKind == Type.ELEMENT) {
      new NamespaceIterator(element)
    } else {
      Collections.EMPTY_LIST.iterator()
    }
  }

  /**
   * Send all the in-scope namespaces for a node (except the XML namespace) to a specified receiver
   * @param element the element in question (the method does nothing if this is not an element)
   * @param receiver the receiver to which the namespaces are notified
   */
  def sendNamespaces(element: NodeInfo, receiver: Receiver) {
    if (element.getNodeKind == Type.ELEMENT) {
      var foundDefault = false
      var iter = iterateNamespaces(element)
      while (iter.hasNext) {
        val nb = iter.next()
        if (nb.getPrefix.isEmpty) {
          foundDefault = true
        }
        receiver.namespace(nb, 0)
      }
      if (!foundDefault) {
        receiver.namespace(NamespaceBinding.DEFAULT_UNDECLARATION, 0)
      }
    }
  }
}

/**
 * This class provides an iterator over the namespace codes representing the in-scope namespaces
 * of any node. It relies on nodes to implement the method
 * {@link client.net.sf.saxon.ce.om.NodeInfo#getDeclaredNamespaces(client.net.sf.saxon.ce.om.NamespaceBinding[])}.
 *
 * <p>The result does not include the XML namespace.</p>
 */
class NamespaceIterator private (var element: NodeInfo) extends Iterator[NamespaceBinding] {

  private var index: Int = 0

  var next: NamespaceBinding = _

  private var localDeclarations: Array[NamespaceBinding] = element.getDeclaredNamespaces(null)

  var undeclaredPrefixes: HashSet[String] = new HashSet(8)

  def hasNext(): Boolean = {
    if (next == null && index != 0) {
      return false
    }
    advance()
    next != null
  }

  private def advance() {
    while (true) {
      var ascend = index >= localDeclarations.length
      var nsCode: NamespaceBinding = null
      if (!ascend) {
        nsCode = localDeclarations(index += 1)
        ascend = nsCode == null
      }
      if (ascend) {
        element = element.getParent
        if (element != null && element.getNodeKind == Type.ELEMENT) {
          localDeclarations = element.getDeclaredNamespaces(localDeclarations)
          index = 0
          //continue
        } else {
          next = null
          return
        }
      }
      val uri = nsCode.getURI
      val prefix = nsCode.getPrefix
      if (uri.isEmpty) {
        undeclaredPrefixes.add(prefix)
      } else {
        if (undeclaredPrefixes.add(prefix)) {
          next = nsCode
          return
        }
      }
    }
  }

  def remove() {
    throw new UnsupportedOperationException()
  }
}

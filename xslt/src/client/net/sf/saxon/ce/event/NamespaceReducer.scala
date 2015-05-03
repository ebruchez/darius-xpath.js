// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.event

import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om.NamespaceBinding
import org.orbeon.darius.xpath.om.NamespaceResolver
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
import java.util.ArrayList
import java.util.Iterator
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * NamespaceReducer is a ProxyReceiver responsible for removing duplicate namespace
 * declarations. It also ensures that an xmlns="" undeclaration is output when
 * necessary. Used on its own, the NamespaceReducer simply eliminates unwanted
 * namespace declarations. It can also be subclassed, in which case the subclass
 * can use the services of the NamespaceReducer to resolve QNames.
 * <p>
 * The NamespaceReducer also validates namespace-sensitive content.
 */
class NamespaceReducer extends ProxyReceiver with NamespaceResolver {

  private var namespaces: Array[NamespaceBinding] = new Array[NamespaceBinding](50)

  private var namespacesSize: Int = 0

  private var countStack: Array[Int] = new Array[Int](50)

  private var depth: Int = 0

  private var disinheritStack: Array[Boolean] = new Array[Boolean](50)

  private var pendingUndeclarations: Array[NamespaceBinding] = null

  /**
   * startElement. This call removes redundant namespace declarations, and
   * possibly adds an xmlns="" undeclaration.
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    nextReceiver.startElement(qName, properties)
    if (depth > 0 && disinheritStack(depth - 1)) {
      pendingUndeclarations = Array.ofDim[NamespaceBinding](namespacesSize)
      System.arraycopy(namespaces, 0, pendingUndeclarations, 0, namespacesSize)
    } else {
      pendingUndeclarations = null
    }
    countStack(depth) = 0
    disinheritStack(depth) = (properties & ReceiverOptions.DISINHERIT_NAMESPACES) != 
      0
    if (depth >= countStack.length) {
      val newstack = Array.ofDim[Int](depth * 2)
      System.arraycopy(countStack, 0, newstack, 0, depth)
      val disStack2 = Array.ofDim[Boolean](depth * 2)
      System.arraycopy(disinheritStack, 0, disStack2, 0, depth)
      countStack = newstack
      disinheritStack = disStack2
    }
    if ((properties & ReceiverOptions.NAMESPACE_OK) == 0) {
      namespace(new NamespaceBinding(qName.getPrefix, qName.getNamespaceURI), 0)
    }
  }

  /**
   * Output a namespace node (binding)
   * @param nsBinding encapsulates the prefix and URI
   * @param properties the properties of the namespace binding
   * @throws XPathException
   */
  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    if (isNeeded(nsBinding)) {
      addToStack(nsBinding)
      countStack(depth - 1) += 1
      nextReceiver.namespace(nsBinding, properties)
    }
  }

  /**
   * Determine whether a namespace declaration is needed
   * @param nscode the namespace code
   * @return true if the namespace is needed: that is, if it not the XML namespace, is not a duplicate,
   * and is not a redundant xmlns="".
   */
  private def isNeeded(nscode: NamespaceBinding): Boolean = {
    if ("xml" == nscode.getPrefix) {
      return false
    }
    if (pendingUndeclarations != null) {
      for (p ← 0 until pendingUndeclarations.length if pendingUndeclarations(p) != null &&
        (nscode.getPrefix == pendingUndeclarations(p).getPrefix)) {
        pendingUndeclarations(p) = null
      }
    }
    var i = namespacesSize - 1
    while (i >= 0) {
      if (namespaces(i) == nscode) {
        return false
      }
      if (namespaces(i).getPrefix == nscode.getPrefix) {
        return true
      }
      i -= 1
    }
    nscode != NamespaceBinding.DEFAULT_UNDECLARATION
  }

  /**
   * Add a namespace declaration to the stack
   * @param nscode the namespace code to be added
   */
  private def addToStack(nscode: NamespaceBinding): Unit = {
    if (namespacesSize + 1 >= namespaces.length) {
      val newlist = Array.ofDim[NamespaceBinding](namespacesSize * 2)
      System.arraycopy(namespaces, 0, newlist, 0, namespacesSize)
      namespaces = newlist
    }
    namespaces(namespacesSize += 1) = nscode
  }

  /**
   * startContent: Add any namespace undeclarations needed to stop
   * namespaces being inherited from parent elements
   */
  def startContent(): Unit = {
    if (pendingUndeclarations != null) {
      for (i ← 0 until pendingUndeclarations.length) {
        val nscode = pendingUndeclarations(i)
        if (nscode != null) {
          namespace(new NamespaceBinding(nscode.getPrefix, ""), 0)
        }
      }
    }
    pendingUndeclarations = null
    nextReceiver.startContent()
  }

  /**
   * endElement: Discard the namespaces declared on this element.
   */
  def endElement(): Unit = {
    if (depth -= 1 == 0) {
      throw new IllegalStateException("Attempt to output end tag with no matching start tag")
    }
    namespacesSize -= countStack(depth)
    nextReceiver.endElement()
  }

  /**
   * Get the namespace URI corresponding to a given prefix. Return null
   * if the prefix is not in scope.
   *
   * @param prefix     the namespace prefix
   * @param useDefault true if the default namespace is to be used when the
   *                   prefix is ""
   * @return the uri for the namespace, or null if the prefix is not in scope
   */
  def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    if (prefix.isEmpty && !useDefault) {
      return NamespaceConstant.NULL
    } else if ("xml" == prefix) {
      return NamespaceConstant.XML
    } else {
      var i = namespacesSize - 1
      while (i >= 0) {
        if (namespaces(i).getPrefix == prefix) {
          return namespaces(i).getURI
        }
        i -= 1
      }
    }
    if (prefix.isEmpty) NamespaceConstant.NULL else null
  }

  /**
   * Get an iterator over all the prefixes declared in this namespace context. This will include
   * the default namespace (prefix="") and the XML namespace where appropriate
   */
  def iteratePrefixes(): Iterator = {
    val prefixes = new ArrayList(namespacesSize)
    var i = namespacesSize - 1
    while (i >= 0) {
      val prefix = namespaces(i).getPrefix
      if (!prefixes.contains(prefix)) {
        prefixes.add(prefix)
      }
      i -= 1
    }
    prefixes.add("xml")
    prefixes.iterator()
  }
}

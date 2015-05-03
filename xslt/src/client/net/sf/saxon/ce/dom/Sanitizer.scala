// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.dom

import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om.Axis
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.pattern.AnyNodeTest
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.trans.StripSpaceRules
import org.orbeon.darius.xpath.tree.iter.UnfailingIterator
import org.orbeon.darius.xpath.tree.util.Navigator
import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.value.Whitespace
import java.util.LinkedList
import java.util.Stack
import scala.collection.JavaConverters._

/**
 * The Sanitizer is responsible for preprocessing a supplied DOM document to make it more suitable for XSLT/XPath
 * processing. The main operation is removal of whitespace-only text nodes as defined using
 * xsl:strip-space and xsl:preserve-space directives in the stylesheet. In future the operation can also potentially
 * merge adjacent text nodes and add namespace information to the tree.
 */
class Sanitizer(var rules: StripSpaceRules) {

  private val xmlSpaceStack = new Stack[String]()

  def sanitize(doc: HTMLDocumentWrapper): Unit = {
    xmlSpaceStack.push("default")
    sanitizeChildren(doc.iterateAxis(Axis.CHILD, NodeKindTest.ELEMENT), strip = false)
  }

  private def sanitizeChildren(iterator: UnfailingIterator, strip: Boolean): Unit = {
    var strippedNodes: LinkedList[HTMLNodeWrapper] = null
    while (true) {
      val node = iterator.next().asInstanceOf[NodeInfo]
      if (node == null) {
        //break
      }
      if (node.getNodeKind == Type.ELEMENT) {
        var xmlSpace = Navigator.getAttributeValue(node, NamespaceConstant.XML, "space")
        if (xmlSpace == null) {
          xmlSpace = xmlSpaceStack.peek()
        }
        xmlSpaceStack.push(xmlSpace)
        val stripChildren = rules.isSpaceStripped(node.getNodeName)
        sanitizeChildren(node.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance), stripChildren)
        xmlSpaceStack.pop()
      } else if (strip && node.getNodeKind == Type.TEXT && xmlSpaceStack.peek() != "preserve" && 
        Whitespace.isWhite(node.getStringValue)) {
        if (strippedNodes == null) {
          strippedNodes = new LinkedList[HTMLNodeWrapper]()
        }
        strippedNodes.addFirst(node.asInstanceOf[HTMLNodeWrapper])
      }
    }
    if (strippedNodes != null) {
      for (node ← strippedNodes.asScala) {
        node.stripTextNode()
      }
    }
  }
}

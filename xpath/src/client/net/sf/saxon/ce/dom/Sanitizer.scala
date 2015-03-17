package client.net.sf.saxon.ce.dom

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.StripSpaceRules
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.Whitespace
import java.util.LinkedList
import java.util.Stack
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The Sanitizer is responsible for preprocessing a supplied DOM document to make it more suitable for XSLT/XPath
 * processing. The main operation is removal of whitespace-only text nodes as defined using
 * xsl:strip-space and xsl:preserve-space directives in the stylesheet. In future the operation can also potentially
 * merge adjacent text nodes and add namespace information to the tree.
 */
class Sanitizer(var rules: StripSpaceRules) {

  private var xmlSpaceStack: Stack[String] = new Stack[String]()

  def sanitize(doc: HTMLDocumentWrapper) {
    xmlSpaceStack.push("default")
    sanitizeChildren(doc.iterateAxis(Axis.CHILD, NodeKindTest.ELEMENT), false)
  }

  private def sanitizeChildren(iterator: UnfailingIterator, strip: Boolean) {
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
      for (node <- strippedNodes) {
        node.stripTextNode()
      }
    }
  }
}

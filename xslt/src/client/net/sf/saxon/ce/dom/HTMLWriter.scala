// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.dom

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.Controller.APIcommand
import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.core.client.JavaScriptException
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.dom.client._
import java.util.logging.Logger
import HTMLWriter._

import scala.beans.BeanProperty

object HTMLWriter {

  private var logger: Logger = Logger.getLogger("XSLT20Processor")

  private /* native */ def attNSSupported(doc: Document): Boolean

  def setAttribute(doc: Document, 
      element: Element, 
      name: String, 
      URI: String, 
      value: String, 
      wMode: WriteMode): Unit = {
    name = tableAttributeFix(name, wMode)
    if (attNSSupported(doc)) {
      setAttributeJs(doc, element, name, URI, value)
    } else {
      val prefix = name.substring(0, name.indexOf(":"))
      val x = "xmlns"
      val nsDeclaraction = if (prefix.length == 0) x else x + ":" + prefix
      if (!element.hasAttribute(nsDeclaraction)) {
        addNamespace(element, prefix, URI)
      }
      element.setAttribute(name, value)
      setAttributeProps(element, name, value)
    }
  }

  def tableAttributeFix(name: String, wMode: WriteMode): String = {
    if (wMode != WriteMode.XML && Configuration.getIeVersion > 0 && 
      name.length > 5) {
      if (name == "rowspan") {
        name = "rowSpan"
      } else if (name == "colspan") {
        name = "colSpan"
      } else if (name == "cellpadding") {
        name = "cellPadding"
      } else if (name == "cellspacing") {
        name = "cellSpacing"
      }
    }
    name
  }

  /**
   *  Creates an attribute with a namespace.
   *  This throws an exception in IE7 that (it seems) must be handled by the caller,
   *  in IE, you can only create a namespace-qualified attribute using the createNode method
   *  of the DOMDocument.
   *
   */
  /* native */ def setAttributeJs(doc: Document, 
      element: Node, 
      name: String, 
      URI: String, 
      value: String): Unit

  private def addNamespace(element: Element, prefix: String, uri: String): Unit = {
    val attName = if (prefix.isEmpty) "xmlns" else "xmlns:" + prefix
    element.setAttribute(attName, uri)
  }

  /**
   * Method for backward compatibility with IE8 and previous where
   * properties and attributes were handled separately
   */
  def setAttributePropsOriginal(element: Element, localName: String, `val`: String): Unit = {
    if (Configuration.getIeVersion > 0 && Configuration.getIeVersion < 9) {
      if (localName.length == 5) {
        if (localName == "style") {
          if (hasStyle(element)) {
            setStyleProperties(element, `val`)
          }
        } else if (localName == "class") {
          setClass(element, `val`)
        } else if (localName == "title") {
          setTitle(element, `val`)
        } else {
          setElementProperty(element, localName, `val`)
        }
      } else if (localName.length == 2 && localName == "id") {
        setId(element, `val`)
      } else {
        setElementProperty(element, localName, `val`)
      }
    }
  }

  /**
   * following setElementProperty method call doesn't work consistently
   * because in IE some element are initially undefined?
   */
  def setAttributeProps(element: Element, localName: String, `val`: String): Unit = {
    if (Configuration.getIeVersion > 0 && Configuration.getIeVersion < 9) {
      if (localName == "style") {
        if (hasStyle(element)) {
          setStyleProperties(element, `val`)
        }
      } else {
        localName = if (localName == "class") "className" else localName
        try {
          setElementProperty(element, localName, `val`)
        } catch {
          case e: Exception ⇒ logger.warning("Unable to set '" + localName + "' property for element.")
        }
      }
    }
  }

  private /* native */ def hasStyle(element: Element): Boolean

  private /* native */ def setClass(element: Element, value: String): Boolean

  private /* native */ def setId(element: Element, value: String): Boolean

  private /* native */ def setTitle(element: Element, value: String): Boolean

  private /* native */ def setElementProperty(element: Element, name: String, value: String): Unit

  /**
   * Parse the value of the style attribute and use it to set individual properties of the style object
   * @param element the element whose style properties are to be updated
   * @param styleAttribute the raw value of the style attribute
   * @throws XPathException
   */
  def setStyleProperties(element: Element, styleAttribute: String): Unit = {
    val semi = styleAttribute.indexOf(';')
    val first = if (semi < 0) styleAttribute else styleAttribute.substring(0, semi)
    val colon = first.indexOf(':')
    if (colon > 0 && colon < first.length - 1) {
      var prop = first.substring(0, colon).trim()
      prop = getCamelCaseName(prop)
      val value = first.substring(colon + 1).trim()
      try {
        element.getStyle.setProperty(prop, value)
      } catch {
        case jex: JavaScriptException ⇒
      }
    }
    if (semi > 0 && semi < styleAttribute.length - 2) {
      setStyleProperties(element, styleAttribute.substring(semi + 1))
    }
  }

  def getCamelCaseName(prop: String): String = {
    while (prop.contains("-")) {
      val h = prop.indexOf('-')
      if (h > 0) {
        var p = prop.substring(0, h) + Character.toUpperCase(prop.charAt(h + 1))
        if (h + 2 < prop.length) {
          p += prop.substring(h + 2)
        }
        prop = p
      }
    }
    prop
  }

  object WriteMode extends Enumeration {

    val NONE = new WriteMode()

    val XML = new WriteMode()

    val HTML = new WriteMode()

    class WriteMode extends Val

    implicit def convertValue(v: Value): WriteMode = v.asInstanceOf[WriteMode]
  }
}

/**
 * DOMWriter is a Receiver that attaches the result tree to a specified Node in the HTML DOM Document
 */
class HTMLWriter extends Receiver {

  private var pipe: PipelineConfiguration = _

  private var currentNode: Node = _

  private var document: Document = _

  private var nextSibling: Node = _

  private var level: Int = 0

  @BeanProperty
  var systemId: String = _

  private var containerNode: Node = _

  /**
   * Native Javascript method to create a namespaced element. Not available in GWT because
   * it's not supported in IE. But needed for SVG/mathML support
   * @param ns the namespace URI
   * @param name the local name
   * @return the constructed element or null if method not available
   */
  private /* native */ def createElementNS(doc: Document, ns: String, name: String): Element

  private /* native */ def createProcessingInstruction(doc: Document, target: String, data: String): Node

  private /* native */ def createComment(doc: Document, data: String): Node

  /**
   * Set the pipelineConfiguration
   */
  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
  }

  /**
   * Get the pipeline configuration used for this document
   */
  def getPipelineConfiguration: PipelineConfiguration = pipe

  /**
   * Start of the document.
   */
  def open(): Unit = {
  }

  /**
   * End of the document.
   */
  def close(): Unit = {
  }

  /**
   * Start of a document node.
   */
  def startDocument(): Unit = {
  }

  /**
   * Notify the end of a document node
   */
  def endDocument(): Unit = {
  }

  /**
   * Start of an element.
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    val localName = qName.getLocalName
    val prefix = qName.getPrefix
    val uri = qName.getNamespaceURI
    var element: Element = null
    if (uri != null && !uri.isEmpty) {
      element = if (mode == WriteMode.XML && prefix != "") createElementNS(document, uri, prefix + ":" + localName) else createElementNS(document, 
        uri, localName)
    }
    if (element == null) {
      element = document.createElement(localName)
    }
    val controller = pipe.getController
    if (controller != null && controller.getApiCommand == APIcommand.UPDATE_HTML && 
      (localName == "html" || localName == "head" || localName == "body")) {
      if (localName == "html") {
        element = document.getFirstChild.asInstanceOf[Element]
      } else {
        element = document.getElementsByTagName(localName.toUpperCase)
          .getItem(0).asInstanceOf[Element]
        val nodes = element.getChildNodes
        for (n ← 0 until nodes.getLength) {
          val node = nodes.getItem(n)
          node.removeFromParent()
        }
      }
      currentNode = element
      level += 1
      return
    }
    if (nextSibling != null && level == 0) {
      currentNode.insertBefore(element, nextSibling)
    } else {
      try {
        currentNode.appendChild(element)
      } catch {
        case err: JavaScriptException ⇒ if (uri == NamespaceConstant.IXSL) {
          val xpe = new XPathException("Error on adding IXSL element to the DOM, the IXSL namespace should be added to the 'extension-element-prefixes' list.")
          throw xpe
        } else {
          throw new XPathException(err.getMessage)
        }
        case exc: Exception ⇒
          val xpe = new XPathException("Error on startElement in HTMLWriter for element '" +
            localName +
            "': " +
            exc.getMessage)
          throw xpe
      }
    }
    currentNode = element
    level += 1
  }

  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    if (mode == WriteMode.XML) {
      val prefix = nsBinding.getPrefix
      val uri = nsBinding.getURI
      val element = currentNode.asInstanceOf[Element]
      if (!(uri == NamespaceConstant.XML)) {
        addNamespace(element, prefix, uri)
      }
    }
  }

  def attribute(nameCode: StructuredQName, value: CharSequence): Unit = {
    var localName = nameCode.getLocalName
    val uri = nameCode.getNamespaceURI
    val `val` = value.toString
    val element = currentNode.asInstanceOf[Element]
    if (mode != WriteMode.XML && NamespaceConstant.HTML_PROP == uri) {
      element.setPropertyString(localName, `val`)
//ORBEON unneeded
//    } else if (mode != WriteMode.XML && NamespaceConstant.HTML_STYLE_PROP == uri) {
//      if (localName.length > 1 && localName.charAt(0) == '_' && localName.charAt(1) == '-') {
//        localName = localName.substring(1)
//      }
//      localName = HTMLWriter.getCamelCaseName(localName)
//      element.getStyle.setProperty(localName, `val`)
    } else if (uri != null && !uri.isEmpty) {
      val fullname = nameCode.getDisplayName
      setAttribute(document, element, fullname, uri, `val`, mode)
    } else {
      localName = tableAttributeFix(localName, mode)
      element.setAttribute(localName, `val`)
      setAttributeProps(element, localName, `val`)
    }
  }

  def startContent(): Unit = {
  }

  /**
   * End of an element.
   */
  def endElement(): Unit = {
    currentNode = currentNode.getParentNode
    level -= 1
  }

  /**
   * Character data.
   */
  def characters(chars: CharSequence): Unit = {
    if (level == 0 && nextSibling == null && Whitespace.isWhite(chars)) {
      return
    }
    try {
      val text = document.createTextNode(chars.toString)
      if (nextSibling != null && level == 0) {
        currentNode.insertBefore(text, nextSibling)
      } else {
        currentNode.appendChild(text)
      }
    } catch {
      case e: Exception ⇒
        val desc = if (nextSibling != null && level == 0) "inserting" else "appending"
        throw new XPathException("DOM error " + desc + " text node with value: '" + chars.toString +
          "' to node with name: " +
          currentNode.getNodeName)
    }
  }

  /**
   * Handle a processing instruction.
   */
  def processingInstruction(target: String, data: CharSequence): Unit = {
    if (mode == WriteMode.XML) {
      val pi = createProcessingInstruction(document, target, data.toString)
      addNode(pi, "processing-instruction")
    }
  }

  /**
   * Handle a comment.
   */
  def comment(chars: CharSequence): Unit = {
    if (mode == WriteMode.XML) {
      val comment = createComment(document, chars.toString)
      addNode(comment, "comment")
    }
  }

  def addNode(newNode: JavaScriptObject, nodeType: String): Unit = {
    try {
      if (nextSibling != null && level == 0) {
        insertBefore(nextSibling, newNode)
      } else {
        appendChild(currentNode, newNode)
      }
    } catch {
      case e: Exception ⇒
        val desc = if (nextSibling != null && level == 0) "inserting" else "appending"
        throw new XPathException("DOM error " + desc + " " + nodeType + " node to node with name: " +
          currentNode.getNodeName)
    }
  }

  /* native */ def appendChild(parent: JavaScriptObject, newChild: JavaScriptObject): JavaScriptObject

  /* native */ def insertBefore(targetNode: JavaScriptObject, newNode: JavaScriptObject): JavaScriptObject

  private var mode: WriteMode = WriteMode.NONE

  /**
   * Set the attachment point for the new subtree
   * @param node the node to which the new subtree will be attached
   */
  def setNode(node: Node): Unit = {
    if (node == null) {
      return
    }
    currentNode = node
    document = if (node.getNodeType == Node.DOCUMENT_NODE) node.asInstanceOf[Document] else currentNode.getOwnerDocument
    if (mode == WriteMode.NONE) {
      val cmd = pipe.getController.getApiCommand
      mode = if (cmd == APIcommand.TRANSFORM_TO_DOCUMENT || cmd == APIcommand.TRANSFORM_TO_FRAGMENT) WriteMode.XML else WriteMode.HTML
    }
  }

  def getNode(): Node = currentNode

  /**
   * Set next sibling
   * @param nextSibling the node, which must be a child of the attachment point, before which the new subtree
   * will be created. If this is null the new subtree will be added after any existing children of the
   * attachment point.
   */
  def setNextSibling(nextSibling: Node): Unit = {
    this.nextSibling = nextSibling
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.dom

import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.dom.HTMLNodeWrapper._
import org.orbeon.darius.xpath.event.Receiver
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.orbeon.ArrayList
import org.orbeon.darius.xpath.pattern.{AnyNodeTest, NameTest, NodeTest}
import org.orbeon.darius.xpath.tree.NamespaceNode
import org.orbeon.darius.xpath.tree.iter.{ArrayIterator, EmptyIterator, SingletonIterator, UnfailingIterator}
import org.orbeon.darius.xpath.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.darius.xpath.value.{AbstractNode, AtomicValue, StringValue, UntypedAtomicValue}
import org.scalajs.dom.{raw ⇒ dom}

import scala.util.control.Breaks

/**
 * A node in the XML parse tree representing an HTML element, character content, or attribute.<P>
 * This is the implementation of the NodeInfo interface used as a wrapper for nodes in the DOM.
 * The DOM itself makes little distinction between XML and HTML, so the parent DocumentNodeWrapper
 * it used to identify the document type for cases where there's a difference - such as namespaces
 * and the capitalisation of element names
 */
class HTMLNodeWrapper protected (protected var node: dom.Node, var parent: HTMLNodeWrapper, protected var index: Int)
    extends AbstractNode with NodeInfo {

  private var qName: StructuredQName = _
  protected var nodeKind: Short = _
  protected var docWrapper: HTMLDocumentWrapper = _
  protected var span: Int = 1

  protected def makeWrapper(node: dom.Node, docWrapper: HTMLDocumentWrapper): HTMLNodeWrapper = {
    if (node == null || docWrapper == null)
      throw new NullPointerException
    makeWrapper(node, docWrapper, null, -1)
  }

  protected def makeWrapper(node: dom.Node,
      docWrapper : HTMLDocumentWrapper,
      parent     : HTMLNodeWrapper,
      index      : Int
  ): HTMLNodeWrapper = {
    var wrapper: HTMLNodeWrapper = null
    node.nodeType match {
      case DocumentNode | DocumentFragmentNode ⇒
        return docWrapper
      case ElementNode ⇒
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.ELEMENT
      case Type.ATTRIBUTE ⇒
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.ATTRIBUTE
      case TextNode ⇒
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.TEXT
      case CDATASectionNode ⇒
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.TEXT
      case Type.COMMENT ⇒
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.COMMENT
      case Type.PROCESSING_INSTRUCTION ⇒
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.PROCESSING_INSTRUCTION
      case _ ⇒
        throw new IllegalArgumentException(s"Unsupported node type in DOM: ${node.nodeType} instance ${node.toString}")
    }
    wrapper.docWrapper = docWrapper
    wrapper
  }

  def getUnderlyingNode: dom.Node = node
  def getNodeKind: Int = nodeKind

  def getTypedValue: AtomicValue = nodeKind match {
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION ⇒ new StringValue(getStringValue)
    case _                                          ⇒ new UntypedAtomicValue(getStringValue)
  }

  // Q: Couldn't this check that the underlying node is the same?
  // DOM Level 3 [says](http://www.w3.org/TR/DOM-Level-3-Core/core.html#Node3-isSameNode):
  //
  // > provides a way to determine whether two Node references returned by the implementation reference the same object.
  // > When two Node references are references to the same object, even if through a proxy, the references may be used
  // > completely interchangeably, such that all attributes have the same values and calling the same DOM method on
  // > either reference always has exactly the same effect
  //
  // Firefox [removed `isSameNode`](https://bugzilla.mozilla.org/show_bug.cgi?id=687400) and seems to indicate that
  // reference equality does the same (in JS, use `===` for strict equality).
  //
  // DOM4 also [removes](http://www.w3.org/TR/dom/#node) `isSameNode`.
  //
  // NOTE: This is called a lot!
  //
  def isSameNodeInfo(other: NodeInfo): Boolean =
    other match {
      case otherWrapper: HTMLNodeWrapper ⇒ node eq otherWrapper.node
      case _                             ⇒ false
    }

  override def equals(other: Any): Boolean = other match {
    case other: NodeInfo ⇒ isSameNodeInfo(other)
    case _               ⇒ false
  }

  override def hashCode(): Int = {
    val buffer = new FastStringBuffer(FastStringBuffer.SMALL)
    generateId(buffer)
    buffer.toString.hashCode
  }

  def getSystemId: String = docWrapper._baseURI

  def setSystemId(uri: String): Unit = {
    docWrapper._baseURI = uri
  }

  def getBaseURI: String = {
    var n: NodeInfo = this
    if (nodeKind != Type.ELEMENT) {
      n = getParent
    }
    while (n ne null) {
      val xmlbase = Navigator.getAttributeValue(n, NamespaceConstant.XML, "base")
      if (xmlbase ne null) {
        return xmlbase
      }
      n = n.getParent
    }
    docWrapper._baseURI
  }

  def compareOrder(other: NodeInfo): Int = Navigator.compareOrder(this, other)

  /**
   * Get the value of the item as a CharSequence. This is in some cases more efficient than
   * the version of the method that returns a String.
   */
  def getStringValue: String = nodeKind match {
    case Type.DOCUMENT | Type.ELEMENT ⇒
      val children1 = node.childNodes
      val sb1 = new StringBuilder(16)
      expandStringValue(children1, sb1)
      sb1.toString
    case Type.ATTRIBUTE ⇒
      emptyIfNull(node.nodeValue)
    case Type.TEXT ⇒
      if (span == 1) {
        emptyIfNull(node.nodeValue)
      } else {
        val fsb = new FastStringBuffer(FastStringBuffer.SMALL)
        var textNode = node
        for (i ← 0 until span) {
          fsb.append(emptyIfNull(textNode.nodeValue))
          textNode = textNode.nextSibling
        }
        fsb.toString
      }
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION ⇒
      emptyIfNull(node.nodeValue)
    case _ ⇒
      ""
  }

  def getNodeName: StructuredQName = {
    if (qName eq null) {
      val nodeKind = getNodeKind
      if (nodeKind == Type.ELEMENT || nodeKind == Type.ATTRIBUTE) {
        var prefix = node.prefix
        if (prefix == null) {
          prefix = ""
        }
        qName = new StructuredQName(prefix, getURI, getLocalPart)
      } else if (nodeKind == Type.PROCESSING_INSTRUCTION) {
        qName = new StructuredQName("", "", getLocalPart)
      }
    }
    qName
  }

  def getRawLocalName: String = getLocalName(node)

  def getLocalPart: String = nodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE ⇒ getDomCorrectedName(getLocalName(node))
    case Type.PROCESSING_INSTRUCTION   ⇒ node.nodeName
    case _                             ⇒ null
  }

  private def getLocalName(node: dom.Node): String =
    if (node.localName ne null) {
      node.localName
    } else {
      val fullname = node.nodeName
      val pos = fullname.indexOf(':')
      if (pos > -1)
        fullname.substring(pos + 1)
      else
        fullname
    }

  def getURI: String = getNodeURI(node, isLocalAttribute = false)

  private def getNodeURI(lNode: dom.Node, isLocalAttribute: Boolean): String = {
    val uri = getNodeNamespace(lNode)
    val isHTML = docWrapper.getDocType == DocTypeHTML
    if (uri == null) {
      ""
    } else if (isHTML && uri == NamespaceConstant.XHTML) {
      ""
    } else {
      uri
    }
  }

  def getDisplayName: String = nodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE ⇒ getDomCorrectedName(node.nodeName)
    case Type.PROCESSING_INSTRUCTION   ⇒ node.nodeName
    case _                             ⇒ ""
  }

  /**
   * Set the name to lower-case if the node is an HTML
   * or XHTML node type - but preserve case for other XML.
   */
  private def getDomCorrectedName(name: String): String = {
    if (name == null) {
      return null
    }
    docWrapper.getDocType match {
      case DocTypeNONHTML ⇒ name
      case DocTypeHTML    ⇒ name.toLowerCase
      case DocTypeXHTML ⇒
        val ns = getURI
        if (ns != null && ns == NamespaceConstant.XHTML) {
          name.toLowerCase
        } else {
          name
        }

      case _ ⇒ if (name.matches("[A-Z]+")) {
        name.toLowerCase
      } else {
        name
      }
    }
  }

  /**
   * Get the NodeInfo object representing the parent of this node
   */
  def getParent: NodeInfo = {
    if (parent == null) {
      parent =
        nodeKind match {
          case Type.ATTRIBUTE ⇒
            throw new IllegalStateException("parent of attribute node is unknown")
          case _ ⇒
            val p = node.parentNode
            if (p == null) {
              null
            } else {
              makeWrapper(p, docWrapper)
            }
        }
    }
    parent
  }

  def getSiblingPosition: Int = {
    if (index == -1) {
      nodeKind match {
        case Type.ELEMENT | Type.TEXT | Type.COMMENT | Type.PROCESSING_INSTRUCTION ⇒
          var ix = 0
          var start = node
          while (true) {
            start = start.previousSibling
            if (start == null) {
              index = ix
              return ix
            }
            ix += 1
          }

        case Type.ATTRIBUTE ⇒
          var ix = 0
          val fp = getNodeName
          val iter = parent.iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
          while (true) {
            val n = iter.next().asInstanceOf[NodeInfo]
            if (n == null || n.getNodeName == fp) {
              index = ix
              return ix
            }
            ix += 1
          }

        case Type.NAMESPACE ⇒
          var ix = 0
          val fp = getNodeName
          val iter = parent.iterateAxis(Axis.NAMESPACE, AnyNodeTest.getInstance)
          while (true) {
            val n = iter.next().asInstanceOf[NodeInfo]
            if (n == null || n.getNodeName == fp) {
              index = ix
              return ix
            }
            ix += 1
          }

        case _ ⇒
          index = 0
      }
    }
    index
  }

  private def iterateAxis(axisNumber: Byte): UnfailingIterator =
    axisNumber match {
      case Axis.ANCESTOR ⇒
        if (nodeKind == Type.DOCUMENT)
          EmptyIterator.getInstance
        else
          Navigator.getAncestorIterator(this, AnyNodeTest.getInstance, includeSelf = false)
      case Axis.ANCESTOR_OR_SELF ⇒
        if (nodeKind == Type.DOCUMENT)
          SingletonIterator.makeIterator(this)
        else
          Navigator.getAncestorIterator(this, AnyNodeTest.getInstance, includeSelf = true)
      case Axis.ATTRIBUTE ⇒
        if (nodeKind != Type.ELEMENT)
          EmptyIterator.getInstance
        else
          new ArrayIterator(getAltAttributes)
      case Axis.CHILD ⇒
        if (hasChildNodes)
          Navigator.newEmptyTextFilter(new ChildEnumeration(this, true, true, false))
        else
          EmptyIterator.getInstance
      case Axis.DESCENDANT ⇒
        if (hasChildNodes)
          new Navigator.DescendantEnumeration(this, false, true)
        else
          EmptyIterator.getInstance
      case Axis.DESCENDANT_OR_SELF ⇒
        new Navigator.DescendantEnumeration(this, true, true)
      case Axis.FOLLOWING ⇒
        new Navigator.FollowingEnumeration(this)
      case Axis.FOLLOWING_SIBLING ⇒
        nodeKind match {
          case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE ⇒ EmptyIterator.getInstance
          case _ ⇒ Navigator.newEmptyTextFilter(new ChildEnumeration(this, false, true, false))
        }
      case Axis.NAMESPACE ⇒
        if (nodeKind != Type.ELEMENT)
          EmptyIterator.getInstance
        else
          NamespaceNode.makeIterator(this, AnyNodeTest.getInstance)
      case Axis.PARENT ⇒
        SingletonIterator.makeIterator(getParent)
      case Axis.PRECEDING ⇒
        new Navigator.PrecedingEnumeration(this, false)
      case Axis.PRECEDING_SIBLING ⇒
        nodeKind match {
          case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE ⇒ EmptyIterator.getInstance
          case _ ⇒ Navigator.newEmptyTextFilter(new ChildEnumeration(this, false, false, false))
        }
      case Axis.SELF ⇒
        SingletonIterator.makeIterator(this)
      case _ ⇒
        throw new IllegalArgumentException
    }

  def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = {
    if (axisNumber == Axis.CHILD && nodeTest.getRequiredNodeKind == Type.ELEMENT) {
      //println("Axis.CHILD and nodeTest.getRequiredNodeKind == Type.ELEMENT ")
      if (hasChildNodes) {
        //println(s"  has child nodes for $nodeTest")
        return Navigator.newAxisFilter(new ChildEnumeration(this, true, true, true), nodeTest)
      } else {
        //println("  has NO child nodes")
        return EmptyIterator.getInstance
      }
    }
    if (docWrapper.getDocType != DocTypeNONHTML && axisNumber == Axis.ATTRIBUTE && nodeTest.isInstanceOf[NameTest]) {
      //println(s"name test $nodeTest")
      if (nodeKind == Type.ELEMENT) {
        val fp = nodeTest.asInstanceOf[NameTest].getRequiredNodeName
        val name = fp.getLocalName
        val value = getAltAttribute(name)
        //println(s"  for element, $fp, $name, $value")
        if (docWrapper.getDocType == DocTypeXHTML) {
          //println(s"  newAxisFilter")
          return Navigator.newAxisFilter(iterateAxis(axisNumber), nodeTest)
        }
        //println(s"  after 1")
        if (value == null || value.length == 0) {
          //println(s"  no value")
          if (getAttributeNames.contains(name)) {
            //println(s"    contains attribute")
            return SingletonIterator.makeIterator(new HTMLAttributeNode(this, name, "", "", ""))
          } else {
            //println(s"    does NOT contains attribute")
            return EmptyIterator.getInstance
          }
        } else {
          //println(s"    some value")
          return SingletonIterator.makeIterator(new HTMLAttributeNode(this, name, "", "", value))
        }
      } else {
        //println(s"    not for an element")
        return EmptyIterator.getInstance
      }
    }
    Navigator.newAxisFilter(iterateAxis(axisNumber), nodeTest)
  }

  /**
   * Workaround for IE issue when getAttribute won't return
   * a value for the style attribute - IE supports non-standard outerHTML
   */
  def getAltAttribute(name: String): String = {
    var value = node.asInstanceOf[dom.Element].getAttribute(name)
    if (name == "class" && value == "") {
      value = node.asInstanceOf[dom.Element].getAttribute("className")
    } else if (name == "for" && value == "") {
      value = node.asInstanceOf[dom.Element].getAttribute("htmlFor")
    }
    value
  }

  private var attributeList: Array[HTMLAttributeNode] = null
  private var attributeNames: ArrayList[String] = null
  private var namespaceBindings: ArrayList[NamespaceBinding] = null

  private def getAttributeNames: ArrayList[String] = {
    if (attributeNames != null) {
      attributeNames
    } else {
      getAltAttributes
      attributeNames
    }
  }

  private def getAltAttributes: Array[HTMLAttributeNode] =
    if (attributeList != null)
      attributeList
    else
      getMainAttributes(node.asInstanceOf[dom.Element])


  private def getMainAttributes(elem: dom.Element): Array[HTMLAttributeNode] = {
    val attributes = node.attributes
    val nodeNames = new ArrayList[String]()
    val len = attributes.length
    val nodeAtts = new ArrayList[HTMLAttributeNode]()
    namespaceBindings = new ArrayList[NamespaceBinding]()
    val getNamespaces = docWrapper.getDocType != DocTypeHTML
    for (i ← 0 until len) {
      val attNode = attributes(i)
      val name = attNode.nodeName
      val `val` = attNode.nodeValue
      if (name.startsWith("xmlns:")) {
        namespaceBindings.add(new NamespaceBinding(name.substring(6), `val`))
      } else if (name == "xmlns") {
        namespaceBindings.add(new NamespaceBinding("", `val`))
      } else {
        if (getNamespaces || name.indexOf(':') > -1) {
          val pfx = attNode.prefix
          var uri = getNodeURI(attNode, isLocalAttribute = true)
          val local = getLocalName(attNode)
          if (uri == null) {
            uri = ""
          }
          nodeAtts.add(new HTMLAttributeNode(this, local, pfx, uri, `val`))
        } else {
          nodeAtts.add(new HTMLAttributeNode(this, name, "", "", `val`))
        }
        nodeNames.add(name)
      }
    }
    val nodes = new Array[HTMLAttributeNode](nodeAtts.size)
    nodeAtts.toArray(nodes)
    attributeList = nodes
    attributeNames = nodeNames
    nodes
  }

  def getRoot: NodeInfo = docWrapper

  def getDocumentRoot: DocumentInfo = docWrapper

  def hasChildNodes: Boolean = {
    nodeKind != Type.ATTRIBUTE && node.hasChildNodes()
  }

  def generateId(buffer: FastStringBuffer) =
    Navigator.appendSequentialKey(this, buffer, addDocNr = true)

  def getDocumentNumber: Int = getDocumentRoot.getDocumentNumber

  def copy(out: Receiver, copyOptions: Int): Unit = {
    ???
//    Navigator.copy(this, out, copyOptions)
  }

  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = {
    if (nodeKind == dom.Node.ELEMENT_NODE) {
      if (namespaceBindings == null) {
        this.getAltAttributes
      }
      val bindings = new Array[NamespaceBinding](namespaceBindings.size)
      namespaceBindings.toArray(bindings)
      bindings
    } else {
      null
    }
  }

  def stripTextNode(): Unit = {
    var textNode = node
    if (span == 1) {
      textNode.parentNode.removeChild(textNode)
    } else {
      for (i ← 0 until span) {
        val t = textNode
        textNode = textNode.nextSibling
        t.parentNode.removeChild(t)
      }
    }
  }

  /**
   * The class ChildEnumeration handles not only the child axis, but also the
   * following-sibling and preceding-sibling axes. It can also iterate the children
   * of the start node in reverse order, something that is needed to support the
   * preceding and preceding-or-ancestor axes (the latter being used by xsl:number)
   */
  private class ChildEnumeration(
    var start        : HTMLNodeWrapper,
    var downwards    : Boolean,
    var forwards     : Boolean,
    var elementsOnly : Boolean
  ) extends UnfailingIterator {

    private val commonParent = if (downwards) start else start.getParent.asInstanceOf[HTMLNodeWrapper]

    var childNodes = commonParent.node.childNodes
    private val childNodesLength = childNodes.length

    private var ix: Int = _

    private var currentSpan: Int = 1

    if (downwards) {
      currentSpan = 1
      ix = if (forwards) -1 else childNodesLength
    } else {
      ix = start.getSiblingPosition
      currentSpan = start.span
    }

    /**
     * Starting with ix positioned at a node, which in the last in a span, calculate the length
     * of the span, that is the number of DOM nodes mapped to this XPath node.
     * @return the number of nodes spanned
     */
    private def skipPrecedingTextNodes(): Int = {
      var count = 0
      import Breaks._
      breakable {
        while (ix >= count) {
          val node = childNodes.item(ix - count)
          val kind = node.nodeType
          if (kind == dom.Node.TEXT_NODE) {
            count += 1
          } else {
            break()
          }
        }
      }
      if (count == 0) 1 else count
    }

    /**
     * Starting with ix positioned at a node, which in the first in a span, calculate the length
     * of the span, that is the number of DOM nodes mapped to this XPath node.
     * @return the number of nodes spanned
     */
    private def skipFollowingTextNodes(): Int = {
      var count = 0
      var pos = ix
      val len = childNodesLength
      import Breaks._
      breakable {
        while (pos < len) {
          val node = childNodes.item(pos)
          val kind = node.nodeType
          if (kind == dom.Node.TEXT_NODE) {
            pos += 1
            count += 1
          } else {
            break()
          }
        }
      }
      if (count == 0) 1 else count
    }

    def next(): Item = {
      import Breaks._
      while (true) {
//        //println(s"next() in ChildEnumeration from $start")
        breakable {
          if (forwards) {
            ix += currentSpan
            if (ix >= childNodesLength) {
              return null
            } else {
              currentSpan = skipFollowingTextNodes()
              val currentDomNode = childNodes.item(ix)
              currentDomNode.nodeType match {
                case Type.PROCESSING_INSTRUCTION ⇒
                  if (elementsOnly || "XML".equalsIgnoreCase(currentDomNode.nodeName))
                    break()
                case Type.DOCUMENT_TYPE ⇒
                  break()
                case ElementNode ⇒
                case _ ⇒ if (elementsOnly) {
                  break()
                }
              }
              val wrapper = makeWrapper(currentDomNode, docWrapper, commonParent, ix)
              wrapper.span = currentSpan
              return wrapper
            }
          } else {
            ix -= 1
            if (ix < 0) {
              return null
            } else {
              currentSpan = skipPrecedingTextNodes()
              ix -= (currentSpan - 1)
              val currentDomNode = childNodes.item(ix)
              currentDomNode.nodeType match {
                case Type.PROCESSING_INSTRUCTION ⇒
                  if (elementsOnly || "XML".equalsIgnoreCase(currentDomNode.nodeName))
                    break()
                case Type.DOCUMENT_TYPE ⇒
                  break()
                case ElementNode ⇒
                case _ ⇒
                  if (elementsOnly)
                    break()
              }
              val wrapper = makeWrapper(currentDomNode, docWrapper, commonParent, ix)
              wrapper.span = currentSpan
              return wrapper
            }
          }
        }
      }
      throw new IllegalStateException
    }

    def getAnother: UnfailingIterator = {
      new ChildEnumeration(start, downwards, forwards, elementsOnly)
    }
  }
}

private object HTMLNodeWrapper {

  val DocumentNode         = dom.Node.DOCUMENT_NODE
  val ElementNode          = dom.Node.ELEMENT_NODE
  val TextNode             = dom.Node.TEXT_NODE
  val DocumentFragmentNode = 11
  val CDATASectionNode     = 4

  def emptyIfNull(s: String): String = if (s == null) "" else s

  def expandStringValue(list: dom.NodeList, sb: StringBuilder): Unit = {
    val len = list.length
    for (i ← 0 until len) {
      val child = list.item(i)
      child.nodeType match {
        case ElementNode ⇒ expandStringValue(child.childNodes, sb)
        case Type.COMMENT | Type.PROCESSING_INSTRUCTION ⇒
        case _ ⇒ sb.append(child.nodeValue)
      }
    }
  }

  def getNodeNamespace(node: dom.Node): String = node.namespaceURI

  def getAttributes(elem: dom.Element): dom.NamedNodeMap = elem.attributes
}
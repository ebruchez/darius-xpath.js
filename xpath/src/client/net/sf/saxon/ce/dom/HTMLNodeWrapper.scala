package client.net.sf.saxon.ce.dom

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper.DocType
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.pattern.NameTest
import client.net.sf.saxon.ce.pattern.NodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.NamespaceNode
import client.net.sf.saxon.ce.tree.iter.ArrayIterator
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AbstractNode
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.StringValue
import client.net.sf.saxon.ce.value.UntypedAtomicValue
import com.google.gwt.core.client.JsArray
import com.google.gwt.dom.client.Element
import com.google.gwt.dom.client.Node
import com.google.gwt.dom.client.NodeList
import com.google.gwt.regexp.shared.MatchResult
import com.google.gwt.regexp.shared.RegExp
import java.util.ArrayList
import HTMLNodeWrapper._
//remove if not needed
import scala.collection.JavaConversions._

object HTMLNodeWrapper {

  private val DOCUMENT_FRAGMENT_NODE = 11

  private val CDATA_SECTION_NODE = 4

  /**
   * Use this alternative as Node class uses nodeValue always -
   * this is now deprecated - there was an attempt to fix pre IE9 problem with
   * reporting values of head child elements - such as title - but
   * this failed with innerHTML property
   * - Issue with IE8 when value is boolean - need to convert to string
   */
  private /* native */ def getValue(n: Node): String

  private /* native */ def getTypeOfNodeValue(n: Node): String

  /**
   * Treat a node value of null as an empty string.
   * @param s the node value
   * @return a zero-length string if s is null, otherwise s
   */
  private def emptyIfNull(s: String): String = (if (s == null) "" else s)

  private def expandStringValue(list: NodeList, sb: StringBuffer) {
    val len = list.getLength
    for (i <- 0 until len) {
      val child = list.getItem(i)
      child.getNodeType match {
        case Node.ELEMENT_NODE => expandStringValue(child.getChildNodes, sb)
        case Type.COMMENT | Type.PROCESSING_INSTRUCTION => //break
        case _ => sb.append(getValue(child))
      }
    }
  }

  /* native */ def getNodeNamespace(node: Node): String

  private /* native */ def getNodePrefix(node: Node): String

  private /* native */ def getOuterHTML(elem: Element): String

  /**
   * Native method to get all attributes for an element as an array of nodes
   * @param elem the element whose attributes are required
   * @return
   */
  private /* native */ def getAttributes(elem: Element): JsArray[Node]
}

/**
 * A node in the XML parse tree representing an HTML element, character content, or attribute.<P>
 * This is the implementation of the NodeInfo interface used as a wrapper for nodes in the DOM.
 * The DOM itself makes little distinction between XML and HTML, so the parent DocumentNodeWrapper
 * it used to identify the document type for cases where there's a difference - such as namespaces
 * and the capitalisation of element names
 */
class HTMLNodeWrapper protected (protected var node: Node, var parent: HTMLNodeWrapper, protected var index: Int)
    extends AbstractNode with NodeInfo {

  private var qName: StructuredQName = _

  protected var nodeKind: Short = _

  protected var docWrapper: HTMLDocumentWrapper = _

  protected var span: Int = 1

  /**
   * Factory method to wrap a DOM node with a wrapper that implements the Saxon
   * NodeInfo interface.
   * @param node        The DOM node
   * @param docWrapper  The wrapper for the containing Document node
   * @return            The new wrapper for the supplied node
   * @throws NullPointerException if the node or the document wrapper are null
   */
  protected def makeWrapper(node: Node, docWrapper: HTMLDocumentWrapper): HTMLNodeWrapper = {
    if (node == null || docWrapper == null) {
      throw new NullPointerException()
    }
    makeWrapper(node, docWrapper, null, -1)
  }

  /**
   * Factory method to wrap a DOM node with a wrapper that implements the Saxon
   * NodeInfo interface.
   * @param node        The DOM node
   * @param docWrapper  The wrapper for the containing Document node     *
   * @param parent      The wrapper for the parent of the JDOM node
   * @param index       The position of this node relative to its siblings
   * @return            The new wrapper for the supplied node
   */
  protected def makeWrapper(node: Node, 
      docWrapper: HTMLDocumentWrapper, 
      parent: HTMLNodeWrapper, 
      index: Int): HTMLNodeWrapper = {
    var wrapper: HTMLNodeWrapper = null
    node.getNodeType match {
      case Node.DOCUMENT_NODE | DOCUMENT_FRAGMENT_NODE => return docWrapper
      case Node.ELEMENT_NODE => 
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.ELEMENT

      case Type.ATTRIBUTE => 
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.ATTRIBUTE

      case Node.TEXT_NODE => 
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.TEXT

      case CDATA_SECTION_NODE => 
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.TEXT

      case Type.COMMENT => 
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.COMMENT

      case Type.PROCESSING_INSTRUCTION => 
        wrapper = new HTMLNodeWrapper(node, parent, index)
        wrapper.nodeKind = Type.PROCESSING_INSTRUCTION

      case _ => throw new IllegalArgumentException("Unsupported node type in DOM! " + node.getNodeType + 
        " instance " + 
        node.toString)
    }
    wrapper.docWrapper = docWrapper
    wrapper
  }

  /**
   * Get the underlying DOM node, to implement the VirtualNode interface
   */
  def getUnderlyingNode(): Node = node

  /**
   * Return the type of node.
   * @return one of the values Node.ELEMENT, Node.TEXT, Node.ATTRIBUTE, etc.
   */
  def getNodeKind(): Int = nodeKind

  /**
   * Get the typed value of the item
   */
  def getTypedValue(): AtomicValue = getNodeKind match {
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION => new StringValue(getStringValue)
    case _ => new UntypedAtomicValue(getStringValue)
  }

  /**
   * Determine whether this is the same node as another node. <br />
   * Note: a.isSameNodeInfo(b) if and only if generateId(a)==generateId(b)
   * @return true if this Node object and the supplied Node object represent the
   * same node in the tree.
   */
  def isSameNodeInfo(other: NodeInfo): Boolean = {
    if (!(other.isInstanceOf[HTMLNodeWrapper])) {
      return false
    }
    val ow = other.asInstanceOf[HTMLNodeWrapper]
    getNodeKind match {
      case Type.ELEMENT => ow.getNodeKind == Type.ELEMENT && getLocalPart == ow.getLocalPart && 
        getURI == ow.getURI && 
        getSiblingPosition == ow.getSiblingPosition && 
        getParent.isSameNodeInfo(ow.getParent)
      case _ => ow.getNodeKind == getNodeKind && getStringValue == ow.getStringValue && 
        getSiblingPosition == ow.getSiblingPosition && 
        getParent.isSameNodeInfo(ow.getParent)
    }
  }

  /**
   * The equals() method compares nodes for identity. It is defined to give the same result
   * as isSameNodeInfo().
   * @param other the node to be compared with this node
   * @return true if this NodeInfo object and the supplied NodeInfo object represent
   *      the same node in the tree.
   * @since 8.7 Previously, the effect of the equals() method was not defined. Callers
   * should therefore be aware that third party implementations of the NodeInfo interface may
   * not implement the correct semantics. It is safer to use isSameNodeInfo() for this reason.
   * The equals() method has been defined because it is useful in contexts such as a Java Set or HashMap.
   */
  override def equals(other: Any): Boolean = other match {
    case other: NodeInfo => isSameNodeInfo(other)
    case _ => false
  }

  /**
   * The hashCode() method obeys the contract for hashCode(): that is, if two objects are equal
   * (represent the same node) then they must have the same hashCode()
   * @since 8.7 Previously, the effect of the equals() and hashCode() methods was not defined. Callers
   * should therefore be aware that third party implementations of the NodeInfo interface may
   * not implement the correct semantics.
   */
  override def hashCode(): Int = {
    val buffer = new FastStringBuffer(FastStringBuffer.SMALL)
    generateId(buffer)
    buffer.toString.hashCode
  }

  /**
   * Get the System ID for the node.
   * @return the System Identifier of the entity in the source document containing the node,
   * or null if not known. Note this is not the same as the base URI: the base URI can be
   * modified by xml:base, but the system ID cannot.
   */
  def getSystemId(): String = docWrapper.baseURI

  def setSystemId(uri: String) {
    docWrapper.baseURI = uri
  }

  /**
   * Get the Base URI for the node, that is, the URI used for resolving a relative URI contained
   * in the node. In the DOM model, base URIs are held only an the document level.
   */
  def getBaseURI(): String = {
    var n = this
    if (getNodeKind != Type.ELEMENT) {
      n = getParent
    }
    while (n != null) {
      val xmlbase = Navigator.getAttributeValue(n, NamespaceConstant.XML, "base")
      if (xmlbase != null) {
        return xmlbase
      }
      n = n.getParent
    }
    docWrapper.baseURI
  }

  /**
   * Determine the relative position of this node and another node, in document order.
   * The other node will always be in the same document.
   * @param other The other node, whose position is to be compared with this node
   * @return -1 if this node precedes the other node, +1 if it follows the other
   * node, or 0 if they are the same node. (In this case, isSameNode() will always
   * return true, and the two nodes will produce the same result for generateId())
   */
  def compareOrder(other: NodeInfo): Int = Navigator.compareOrder(this, other)

  /**
   * Get the value of the item as a CharSequence. This is in some cases more efficient than
   * the version of the method that returns a String.
   */
  def getStringValue(): String = nodeKind match {
    case Type.DOCUMENT | Type.ELEMENT => 
      var children1 = node.getChildNodes
      var sb1 = new StringBuffer(16)
      expandStringValue(children1, sb1)
      sb1.toString

    case Type.ATTRIBUTE => emptyIfNull(getValue(node))
    case Type.TEXT => if (span == 1) {
      emptyIfNull(getValue(node))
    } else {
      val fsb = new FastStringBuffer(FastStringBuffer.SMALL)
      var textNode = node
      for (i <- 0 until span) {
        fsb.append(emptyIfNull(getValue(textNode)))
        textNode = textNode.getNextSibling
      }
      fsb.toString
    }
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION => emptyIfNull(getValue(node))
    case _ => ""
  }

  /**
   * Get name code. The name code is a coded form of the node name: two nodes
   * with the same name code have the same namespace URI, the same local name,
   * and the same prefix. By masking the name code with &0xfffff, you get a
   * fingerprint: two nodes with the same fingerprint have the same local name
   * and namespace URI.
   */
  def getNodeName(): StructuredQName = {
    if (qName != null) {
      return qName
    }
    val nodeKind = getNodeKind
    if (nodeKind == Type.ELEMENT || nodeKind == Type.ATTRIBUTE) {
      var prefix = getNodePrefix(node)
      if (prefix == null) {
        prefix = ""
      }
      qName = new StructuredQName(prefix, getURI, getLocalPart)
      qName
    } else if (nodeKind == Type.PROCESSING_INSTRUCTION) {
      qName = new StructuredQName("", "", getLocalPart)
      qName
    } else {
      null
    }
  }

  /**
   * Get the local part of the name of this node. This is the name after the ":" if any.
   * @return the local part of the name. For an unnamed node, returns null, except for
   * un unnamed namespace node, which returns "".
   */
  def getRawLocalName(): String = getLocalName(node)

  def getLocalPart(): String = getNodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE => getDomCorrectedName(getLocalName(node))
    case Type.PROCESSING_INSTRUCTION => node.getNodeName
    case _ => null
  }

  private /* native */ def getLocalName(node: Node): String

  /**
   * Get the URI part of the name of this node. This is the URI corresponding to the
   * prefix, or the URI of the default namespace if appropriate.
   * @return The URI of the namespace of this node. For an unnamed node,
   *     or for a node with an empty prefix, return an empty
   *     string.
   */
  def getURI(): String = getNodeURI(node, false)

  private def getNodeURI(lNode: Node, isLocalAttribute: Boolean): String = {
    var element: NodeInfo = null
    if (isLocalAttribute || nodeKind == Type.ELEMENT) {
      element = this
    } else if (nodeKind == Type.ATTRIBUTE) {
      element = this.getParent
    } else {
      return ""
    }
    val uri = getNodeNamespace(lNode)
    val isHTML = (docWrapper.getDocType == DocType.HTML)
    if (uri == null) {
      ""
    } else if (isHTML && uri == NamespaceConstant.XHTML) {
      ""
    } else {
      uri
    }
  }

  /**
   * Get the display name of this node. For elements and attributes this is [prefix:]localname.
   * For unnamed nodes, it is an empty string.
   * @return The display name of this node.
   * For a node with no name, return an empty string.
   */
  def getDisplayName(): String = nodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE => getDomCorrectedName(node.getNodeName)
    case Type.PROCESSING_INSTRUCTION => node.getNodeName
    case _ => ""
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
      case NONHTML => name
      case HTML => name.toLowerCase()
      case XHTML => 
        var ns = getURI
        if (ns != null && ns == NamespaceConstant.XHTML) {
          name.toLowerCase()
        } else {
          name
        }

      case _ => if (name.matches("[A-Z]+")) {
        name.toLowerCase()
      } else {
        name
      }
    }
  }

  /**
   * Get the NodeInfo object representing the parent of this node
   */
  def getParent(): NodeInfo = {
    if (parent == null) getNodeKind match {
      case Type.ATTRIBUTE => throw new IllegalStateException("parent of attribute node is unknown")
      case _ => 
        var p = node.getParentNode
        if (p == null) {
          return null
        } else {
          parent = makeWrapper(p, docWrapper)
        }

    }
    parent
  }

  /**
   * Get the index position of this node among its siblings (starting from 0).
   * In the case of a text node that maps to several adjacent siblings in the DOM,
   * the numbering actually refers to the position of the underlying DOM nodes;
   * thus the sibling position for the text node is that of the first DOM node
   * to which it relates, and the numbering of subsequent XPath nodes is not necessarily
   * consecutive.
   */
  def getSiblingPosition(): Int = {
    if (index == -1) nodeKind match {
      case Type.ELEMENT | Type.TEXT | Type.COMMENT | Type.PROCESSING_INSTRUCTION => 
        var ix = 0
        var start = node
        while (true) {
          start = start.getPreviousSibling
          if (start == null) {
            index = ix
            return ix
          }
          ix += 1
        }

      case Type.ATTRIBUTE => 
        ix = 0
        var fp = getNodeName
        var iter = parent.iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
        while (true) {
          val n = iter.next().asInstanceOf[NodeInfo]
          if (n == null || n.getNodeName == fp) {
            index = ix
            return ix
          }
          ix += 1
        }

      case Type.NAMESPACE => 
        ix = 0
        fp = getNodeName
        iter = parent.iterateAxis(Axis.NAMESPACE, AnyNodeTest.getInstance)
        while (true) {
          val n = iter.next().asInstanceOf[NodeInfo]
          if (n == null || n.getNodeName == fp) {
            index = ix
            return ix
          }
          ix += 1
        }

      case _ => 
        index = 0
        return index

    }
    index
  }

  /**
   * Return an iteration over the nodes reached by the given axis from this node
   * @param axisNumber the axis to be used
   * @return a SequenceIterator that scans the nodes reached by the axis in turn.
   */
  private def iterateAxis(axisNumber: Byte): UnfailingIterator = axisNumber match {
    case Axis.ANCESTOR => 
      if (nodeKind == Type.DOCUMENT) {
        EmptyIterator.getInstance
      }
      Navigator.getAncestorIterator(this, AnyNodeTest.getInstance, false)

    case Axis.ANCESTOR_OR_SELF => 
      if (nodeKind == Type.DOCUMENT) {
        SingletonIterator.makeIterator(this)
      }
      Navigator.getAncestorIterator(this, AnyNodeTest.getInstance, true)

    case Axis.ATTRIBUTE => 
      if (nodeKind != Type.ELEMENT) {
        EmptyIterator.getInstance
      }
      var nodes = getAltAttributes
      new ArrayIterator(nodes)

    case Axis.CHILD => if (hasChildNodes()) {
      Navigator.newEmptyTextFilter(new ChildEnumeration(this, true, true, false))
    } else {
      EmptyIterator.getInstance
    }
    case Axis.DESCENDANT => if (hasChildNodes()) {
      new Navigator.DescendantEnumeration(this, false, true)
    } else {
      EmptyIterator.getInstance
    }
    case Axis.DESCENDANT_OR_SELF => new Navigator.DescendantEnumeration(this, true, true)
    case Axis.FOLLOWING => new Navigator.FollowingEnumeration(this)
    case Axis.FOLLOWING_SIBLING => nodeKind match {
      case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE => EmptyIterator.getInstance
      case _ => Navigator.newEmptyTextFilter(new ChildEnumeration(this, false, true, false))
    }
    case Axis.NAMESPACE => 
      if (nodeKind != Type.ELEMENT) {
        EmptyIterator.getInstance
      }
      NamespaceNode.makeIterator(this, AnyNodeTest.getInstance)

    case Axis.PARENT => 
      getParent
      SingletonIterator.makeIterator(parent)

    case Axis.PRECEDING => new Navigator.PrecedingEnumeration(this, false)
    case Axis.PRECEDING_SIBLING => nodeKind match {
      case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE => EmptyIterator.getInstance
      case _ => Navigator.newEmptyTextFilter(new ChildEnumeration(this, false, false, false))
    }
    case Axis.SELF => SingletonIterator.makeIterator(this)
    case _ => throw new IllegalArgumentException()
  }

  /**
   * Return an iteration over the nodes reached by the given axis from this node
   * @param axisNumber the axis to be used
   * @param nodeTest A pattern to be matched by the returned nodes
   * @return a SequenceIterator that scans the nodes reached by the axis in turn.
   */
  def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = {
    if (axisNumber == Axis.CHILD && nodeTest.getRequiredNodeKind == Type.ELEMENT) {
      if (hasChildNodes()) {
        return Navigator.newAxisFilter(new ChildEnumeration(this, true, true, true), nodeTest)
      } else {
        return EmptyIterator.getInstance
      }
    }
    if (docWrapper.getDocType != DocType.NONHTML && axisNumber == Axis.ATTRIBUTE && 
      nodeTest.isInstanceOf[NameTest]) {
      if (nodeKind == Type.ELEMENT) {
        val fp = nodeTest.asInstanceOf[NameTest].getRequiredNodeName
        val uri = fp.getNamespaceURI
        var name = fp.getLocalName
        var value: String = null
        if (NamespaceConstant.HTML_PROP == uri) {
          value = node.asInstanceOf[Element].getPropertyString(name)
        } else if (NamespaceConstant.HTML_STYLE_PROP == uri) {
          if (name.length > 1 && name.charAt(0) == '_' && name.charAt(1) == '-') {
            name = name.substring(1)
          }
          name = HTMLWriter.getCamelCaseName(name)
          value = node.asInstanceOf[Element].getStyle.getProperty(name)
        } else {
          value = getAltAttribute(name)
          if (docWrapper.getDocType == DocType.XHTML) {
            return Navigator.newAxisFilter(iterateAxis(axisNumber), nodeTest)
          }
        }
        if (value == null || value.length == 0) {
          if (getAttributeNames.contains(name)) {
            return SingletonIterator.makeIterator(new HTMLAttributeNode(this, name, "", "", ""))
          } else {
            return EmptyIterator.getInstance
          }
        } else {
          return SingletonIterator.makeIterator(new HTMLAttributeNode(this, name, "", "", value))
        }
      } else {
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
    var value = node.asInstanceOf[Element].getAttribute(name)
    if (value == "[object]" && name == "style") {
      value = getStyleAttribute(node.asInstanceOf[Element])
    } else if (name == "class" && value == "") {
      value = node.asInstanceOf[Element].getAttribute("className")
    } else if (name == "for" && value == "") {
      value = node.asInstanceOf[Element].getAttribute("htmlFor")
    }
    value
  }

  private def getStyleAttribute(elem: Element): String = {
    val value = getOuterHTML(elem)
    val quotesPattern = RegExp.compile("(?:\".*?\"|\'.*?\'|[^\'\"]+|['\"])", "g")
    val stylePattern = RegExp.compile("[\\s]style\\s*=")
    var m = quotesPattern.exec(value)
    var i = 0
    var styleFound = false
    var styleContent = ""
    var nonQ = ""
    while (quotesPattern.getLastIndex > 0) {
      if (i % 2 == 0) {
        nonQ = m.getGroup(0)
        val ms = stylePattern.exec(nonQ)
        styleFound = ms.getGroupCount > 0
        if (!styleFound && nonQ.indexOf('>') > -1) {
          //break
        }
      } else if (styleFound) {
        styleContent = m.getGroup(0)
        styleContent = styleContent.substring(1, styleContent.length - 1)
        //break
      }
      i += 1
      m = quotesPattern.exec(value)
    }
    styleContent
  }

  private def skipWhitespace(pos: Int, value: String, invert: Boolean): Array[Int] = {
    var offset = 0
    var ch = '\0'
    while (true) {
      ch = value.charAt(pos + offset)
      var isWhitespace = (ch == ' ' || ch == '\r' || ch == '\n' || ch == '\t')
      if (invert) {
        isWhitespace = !isWhitespace
      }
      if (!(isWhitespace)) {
        //break
      }
      offset += 1
      if (!(pos + offset < value.length)) {
        ch = '\0'
        //break
      }
    }
    Array(offset, ch)
  }

  /**
   * Alternate to Native method to get all attributes for an element as an array of nodes
   * @param elem the element whose attributes are required. This alternate is required
   * because of sketchy support for the attributes property in various browsers including IE
   * @return
   */
  private var attributeList: Array[HTMLAttributeNode] = null

  private var attributeNames: ArrayList[String] = null

  private var namespaceBindings: ArrayList[NamespaceBinding] = null

  private def getAttributeNames(): ArrayList[String] = {
    if (attributeNames != null) {
      attributeNames
    } else {
      getAltAttributes
      attributeNames
    }
  }

  private def getAltAttributes(): Array[HTMLAttributeNode] = {
    if (attributeList != null) {
      return attributeList
    }
    val elem = node.asInstanceOf[Element]
    val ieVersion = Configuration.getIeVersion
    if (ieVersion < 0 || ieVersion > 8) {
      return getMainAttributes(elem)
    }
    val value = getOuterHTML(elem)
    if (value == null) {
      return getMainAttributes(elem)
    }
    val nodeNames = new ArrayList[String]()
    val xmlnsNames = new ArrayList[String]()
    val xmlnsUris = new ArrayList[String]()
    namespaceBindings = new ArrayList[NamespaceBinding]()
    val quotesPattern = RegExp.compile("(?:\"(.|\n)*?\"|\'(.|\n)*?\'|[^\'\"]+|['\"])", "gm")
    var m = quotesPattern.exec(value)
    var i = 0
    var nonQ = ""
    var awaitingXmlnsUri = false
    while (quotesPattern.getLastIndex > 0) {
      if (i % 2 == 0) {
        nonQ = m.getGroup(0)
        var sb = new StringBuffer()
        var endOfTag = false
        var isName = !(i == 0)
        var start = 0
        if (i == 0) {
          start = nonQ.indexOf('<') + 1
        }
        for (x <- start until nonQ.length) {
          val offsetChar = skipWhitespace(x, nonQ, false)
          val offset = offsetChar(0)
          if (offset > 0 && !(isName)) {
            isName = true
          }
          val ch = offsetChar(1).toChar
          if (ch == '\0') //break
          if (ch == '=') {
            isName = false
            val attName = sb.toString
            if (attName.startsWith("xmlns")) {
              xmlnsNames.add(attName)
              awaitingXmlnsUri = true
            } else if (attName.length != 0) {
              nodeNames.add(attName)
              awaitingXmlnsUri = false
            }
            sb = new StringBuffer()
            //continue
          } else if (ch == '>') {
            endOfTag = true
            //break
          }
          if (isName) {
            sb.append(ch)
          }
          x += offset
        }
        if (endOfTag) {
          //break
        }
      } else if (awaitingXmlnsUri) {
        xmlnsUris.add(m.getGroup(0))
      }
      i += 1
      m = quotesPattern.exec(value)
    }
    val nodeArray = Array.ofDim[HTMLAttributeNode](nodeNames.size)
    for (y <- 0 until nodeNames.size) {
      val name = nodeNames.get(y)
      val nValue = getAltAttribute(name)
      val hNode = new HTMLAttributeNode(this, name, "", "", nValue)
      nodeArray(y) = hNode
    }
    var count = 0
    for (name <- xmlnsNames) {
      val noPrefix = (name.length == 5)
      val prefix = if ((noPrefix)) "" else name.substring(6)
      val attValue = if ((noPrefix)) getAltAttribute(name) else trimEdgeChars(xmlnsUris.get(count))
      namespaceBindings.add(new NamespaceBinding(prefix, attValue))
      count += 1
    }
    attributeList = nodeArray
    attributeNames = nodeNames
    nodeArray
  }

  private def trimEdgeChars(name: String): String = name.substring(1, name.length - 1)

  private def getMainAttributes(elem: Element): Array[HTMLAttributeNode] = {
    val attributes = getAttributes(node.asInstanceOf[Element])
    val nodeNames = new ArrayList[String]()
    val len = attributes.length
    val nodeAtts = new ArrayList[HTMLAttributeNode]()
    namespaceBindings = new ArrayList[NamespaceBinding]()
    val getNamespaces = (docWrapper.getDocType != DocType.HTML)
    for (i <- 0 until len) {
      val attNode = attributes.get(i)
      val name = attNode.getNodeName
      var `val` = ""
      try {
        `val` = getValue(attNode)
      } catch {
        case e: Exception => {
          val rp = getTypeOfNodeValue(attNode)
          val a = "a"
        }
      }
      if (name.startsWith("xmlns:")) {
        namespaceBindings.add(new NamespaceBinding(name.substring(6), `val`))
      } else if (name == "xmlns") {
        namespaceBindings.add(new NamespaceBinding("", `val`))
      } else {
        if (getNamespaces || name.indexOf(':') > -1) {
          val pfx = getNodePrefix(attNode)
          var uri = getNodeURI(attNode, true)
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
    val nodes = Array.ofDim[HTMLAttributeNode](nodeAtts.size)
    nodeAtts.toArray(nodes)
    attributeList = nodes
    attributeNames = nodeNames
    nodes
  }

  /**
   * Get the root node - always a document node with this tree implementation
   * @return the NodeInfo representing the containing document
   */
  def getRoot(): NodeInfo = docWrapper

  /**
   * Get the root (document) node
   * @return the DocumentInfo representing the containing document
   */
  def getDocumentRoot(): DocumentInfo = docWrapper

  /**
   * Determine whether the node has any children. <br />
   * Note: the result is equivalent to <br />
   * getEnumeration(Axis.CHILD, AnyNodeTest.getInstance()).hasNext()
   */
  def hasChildNodes(): Boolean = {
    nodeKind != Type.ATTRIBUTE && node.hasChildNodes()
  }

  /**
   * Get a character string that uniquely identifies this node.
   * Note: a.isSameNode(b) if and only if generateId(a)==generateId(b)
   * @param buffer a buffer to contain a string that uniquely identifies this node, across all
   * documents
   *
   */
  def generateId(buffer: FastStringBuffer) {
    Navigator.appendSequentialKey(this, buffer, true)
  }

  /**
   * Get the document number of the document containing this node. For a free-standing
   * orphan node, just return the hashcode.
   */
  def getDocumentNumber(): Int = getDocumentRoot.getDocumentNumber

  /**
   * Copy this node to a given outputter (deep copy)
   */
  def copy(out: Receiver, copyOptions: Int) {
    Navigator.copy(this, out, copyOptions)
  }

  /**
   * Get all namespace undeclarations and undeclarations defined on this element.
   *
   * @param buffer If this is non-null, and the result array fits in this buffer, then the result
   *               may overwrite the contents of this array, to avoid the cost of allocating a new array on the heap.
   * @return An array of integers representing the namespace declarations and undeclarations present on
   *         this element. For a node other than an element, return null. Otherwise, the returned array is a
   *         sequence of namespace codes, whose meaning may be interpreted by reference to the name pool. The
   *         top half word of each namespace code represents the prefix, the bottom half represents the URI.
   *         If the bottom half is zero, then this is a namespace undeclaration rather than a declaration.
   *         The XML namespace is never included in the list. If the supplied array is larger than required,
   *         then the first unused entry will be set to -1.
   *         <p/>
   *         <p>For a node other than an element, the method returns null.</p>
   */
  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = {
    if (nodeKind == Node.ELEMENT_NODE) {
      if (namespaceBindings == null) {
        this.getAltAttributes
      }
      val bindings = Array.ofDim[NamespaceBinding](namespaceBindings.size)
      namespaceBindings.toArray(bindings)
      bindings
    } else {
      null
    }
  }

  /**
   * Strip a node from the tree (used when stripping whitespace text nodes). This method needs care
   * because it invalidates the index numbers of siblings of the removed node.
   */
  def stripTextNode() {
    var textNode = node
    if (span == 1) {
      textNode.removeFromParent()
    } else {
      for (i <- 0 until span) {
        val t = textNode
        textNode = textNode.getNextSibling
        t.removeFromParent()
      }
    }
  }

  /**
   * The class ChildEnumeration handles not only the child axis, but also the
   * following-sibling and preceding-sibling axes. It can also iterate the children
   * of the start node in reverse order, something that is needed to support the
   * preceding and preceding-or-ancestor axes (the latter being used by xsl:number)
   */
  private class ChildEnumeration(var start: HTMLNodeWrapper, 
      var downwards: Boolean, 
      var forwards: Boolean, 
      var elementsOnly: Boolean) extends UnfailingIterator {

    private var commonParent: HTMLNodeWrapper = if (downwards) start else start.getParent.asInstanceOf[HTMLNodeWrapper]

    var childNodes: NodeList = commonParent.node.getChildNodes

    private var childNodesLength: Int = childNodes.getLength

    private var ix: Int = _

    private var currentSpan: Int = 1

    if (downwards) {
      currentSpan = 1
      ix = (if (forwards) -1 else childNodesLength)
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
      while (ix >= count) {
        val node = childNodes.getItem(ix - count)
        val kind = node.getNodeType
        if (kind == Node.TEXT_NODE) {
          count += 1
        } else {
          //break
        }
      }
      (if (count == 0) 1 else count)
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
      while (pos < len) {
        val node = childNodes.getItem(pos)
        val kind = node.getNodeType
        if (kind == Node.TEXT_NODE) {
          pos += 1
          count += 1
        } else {
          //break
        }
      }
      (if (count == 0) 1 else count)
    }

    def next(): Item = {
      while (true) {
        if (forwards) {
          ix += currentSpan
          if (ix >= childNodesLength) {
            null
          } else {
            currentSpan = skipFollowingTextNodes()
            val currentDomNode = childNodes.getItem(ix)
            currentDomNode.getNodeType match {
              case Type.PROCESSING_INSTRUCTION => if (elementsOnly || "XML".equalsIgnoreCase(currentDomNode.getNodeName)) {
                //continue
              } else {
                //break
              }
              case Type.DOCUMENT_TYPE => //continue
              case Node.ELEMENT_NODE => //break
              case _ => if (elementsOnly) {
                //continue
              } else {
                //break
              }
            }
            val wrapper = makeWrapper(currentDomNode, docWrapper, commonParent, ix)
            wrapper.span = currentSpan
            wrapper
          }
        } else {
          ix -= 1
          if (ix < 0) {
            null
          } else {
            currentSpan = skipPrecedingTextNodes()
            ix -= (currentSpan - 1)
            val currentDomNode = childNodes.getItem(ix)
            currentDomNode.getNodeType match {
              case Type.PROCESSING_INSTRUCTION => if (elementsOnly || "XML".equalsIgnoreCase(currentDomNode.getNodeName)) {
                //continue
              } else {
                //break
              }
              case Type.DOCUMENT_TYPE => //continue
              case Node.ELEMENT_NODE => //break
              case _ => if (elementsOnly) {
                //continue
              } else {
                //break
              }
            }
            val wrapper = makeWrapper(currentDomNode, docWrapper, commonParent, ix)
            wrapper.span = currentSpan
            wrapper
          }
        }
      }
    }

    def getAnother(): UnfailingIterator = {
      new ChildEnumeration(start, downwards, forwards, elementsOnly)
    }
  }
}

package client.net.sf.saxon.ce.dom

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.`type`.Type
import com.google.gwt.dom.client.Document
import com.google.gwt.dom.client.Node
import java.util.HashMap
import HTMLDocumentWrapper._
//remove if not needed
import scala.collection.JavaConversions._

object HTMLDocumentWrapper {

  object DocType extends Enumeration {

    val XHTML = new DocType()

    val HTML = new DocType()

    val UNKNOWN = new DocType()

    val NONHTML = new DocType()

    class DocType extends Val

    implicit def convertValue(v: Value): DocType = v.asInstanceOf[DocType]
  }

  /* native */ def supportsGetElementById(doc: Document): Boolean

  /* native */ def getXmlIdNS(inNode: Node): String

  /* native */ def getXmlId(inNode: Node): String

  private /* native */ def isNSok(inNode: Node): Boolean

  /**
   * Create a DocumentFragment node. Method not available from GWT
   */
  /* native */ def createDocumentFragment(doc: Document): Node
}

/**
 * The document node of a tree implemented as a wrapper around an XML DOM Document.
 */
class HTMLDocumentWrapper(doc: Node, 
    baseURI: String, 
    config: Configuration, 
    newDocType: DocType) extends HTMLNodeWrapper(doc, null, 0) with DocumentInfo {

  protected var config: Configuration = _

  protected var baseURI: String = _

  protected var documentNumber: Int = _

  protected var domLevel3: Boolean = true

  private var userData: HashMap[String, Any] = _

  private var idIndex: HashMap[String, HTMLNodeWrapper] = _

  private var isHttpRequested: Boolean = (newDocType == DocType.NONHTML)

  nodeKind = Type.DOCUMENT

  if ((baseURI == null || baseURI == "") && doc.getNodeType == Type.DOCUMENT) {
    baseURI = doc.asInstanceOf[Document].getURL
    this.baseURI = if ((baseURI != null && baseURI != "")) baseURI else getBaseURI(doc.asInstanceOf[Document])
  } else {
    this.baseURI = baseURI
  }

  docWrapper = this

  setConfiguration(config)

  if (newDocType != DocType.UNKNOWN) {
    this.htmlType = newDocType
    return
  }

  try {
    val iter = this.iterateAxis(Axis.CHILD, NodeKindTest.ELEMENT)
    while (true) {
      val n = iter.next().asInstanceOf[NodeInfo]
      if (n == null) {
        //break
      } else {
        val rawLocal = n.asInstanceOf[HTMLNodeWrapper].getRawLocalName.toLowerCase()
        if (rawLocal == "html") {
          val nb = n.getDeclaredNamespaces(null)
          htmlType = DocType.HTML
          for (nBinding <- nb if nBinding.getURI == NamespaceConstant.XHTML) {
            htmlType = DocType.XHTML
            //break
          }
        } else {
          htmlType = DocType.NONHTML
        }
        //break
      }
    }
  } catch {
    case e: Exception => 
  }

  /**
   * Wrap a DOM Document or DocumentFragment node
   * @param doc a DOM Document or DocumentFragment node
   * @param baseURI the base URI of the document
   * @param config the Saxon configuration
   */
  def this(doc: Node, baseURI: String, config: Configuration) {
    this(doc, baseURI, config, DocType.UNKNOWN)
  }

  /* native */ def getBaseURI(doc: Document): String

  private var htmlType: DocType = DocType.UNKNOWN

  private def getIsXMLObjectTypeFromURIext(): Boolean = {
    val pos = baseURI.indexOf('?')
    val testUri = if ((pos < 0)) baseURI else baseURI.substring(pos)
    testUri.toLowerCase().endsWith("xhtml")
  }

  /**
   * @return type of document determined by tag name
   */
  def getDocType(): DocType = htmlType

  /**
   * @return type of document determined by object type
   */
  def isXMLDocumentObject(): Boolean = isHttpRequested

  private def isNodeXMLDocument(): Boolean = {
    val nodeString = node.toString
    if (nodeString.endsWith("XMLDocument]")) {
      true
    } else if (nodeString.endsWith("HTMLDocument]")) {
      false
    } else {
      !supportsGetElementById(node.asInstanceOf[Document])
    }
  }

  /**
   * Create a wrapper for a node in this document
   *
   * @param node the DOM node to be wrapped. This must be a node within the document wrapped by this
   *             XMLDocumentWrapper
   * @throws IllegalArgumentException if the node is not a descendant of the Document node wrapped by
   *                                  this XMLDocumentWrapper
   */
  def wrap(node: Node): HTMLNodeWrapper = {
    if (node == this.node) {
      return this
    }
    val doc = node.getOwnerDocument
    if (doc == this.node) {
      makeWrapper(node, this)
    } else {
      throw new IllegalArgumentException("XMLDocumentWrapper#wrap: supplied node does not belong to the wrapped DOM document")
    }
  }

  /**
   * Set the Configuration that contains this document
   */
  def setConfiguration(config: Configuration) {
    this.config = config
    documentNumber = config.allocateDocumentNumber()
  }

  /**
   * Get the configuration previously set using setConfiguration
   */
  def getConfiguration(): Configuration = config

  /**
   * Get the unique document number
   */
  def getDocumentNumber(): Int = documentNumber

  /**
   * Get the element with a given ID, if any
   *
   * @param id the required ID value
   * @return a NodeInfo representing the element with the given ID, or null if there
   *         is no such element. This relies on the getElementById() method in the
   *         underlying DOM.
   */
  def selectID(id: String): NodeInfo = {
    var el: Node = null
    val doc = node.asInstanceOf[Document]
    if (!isHttpRequested) {
      el = (doc).getElementById(id)
      if (el == null) {
        return null
      }
      wrap(el)
    } else {
      if (idIndex != null) {
        idIndex.get(id)
      } else {
        idIndex = new HashMap()
        val iter = iterateAxis(Axis.DESCENDANT, NodeKindTest.ELEMENT)
        val useNS = isNSok(node)
        while (true) {
          val node = iter.next().asInstanceOf[NodeInfo]
          if (node == null) {
            //break
          }
          val testNode = node.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode
          val xmlId = if ((useNS)) getXmlIdNS(testNode) else getXmlId(testNode)
          if (xmlId != null && !xmlId.isEmpty) {
            idIndex.put(xmlId, node.asInstanceOf[HTMLNodeWrapper])
          }
        }
        idIndex.get(id)
      }
    }
  }

  /**
   * Determine whether this is the same node as another node. <br />
   * Note: a.isSameNode(b) if and only if generateId(a)==generateId(b)
   *
   * @return true if this Node object and the supplied Node object represent the
   *         same node in the tree.
   */
  def isSameNodeInfo(other: NodeInfo): Boolean = {
    other.isInstanceOf[HTMLDocumentWrapper] && 
      node == other.asInstanceOf[HTMLDocumentWrapper].node
  }

  /**
   * Set user data on the document node. The user data can be retrieved subsequently
   * using {@link #getUserData}
   * @param key   A string giving the name of the property to be set. Clients are responsible
   *              for choosing a key that is likely to be unique. Must not be null.
   * @param value The value to be set for the property. May be null, which effectively
   *              removes the existing value for the property.
   */
  def setUserData(key: String, value: AnyRef) {
    if (userData == null) {
      userData = new HashMap(4)
    }
    if (value == null) {
      userData.remove(key)
    } else {
      userData.put(key, value)
    }
  }

  /**
   * Get user data held in the document node. This retrieves properties previously set using
   * {@link #setUserData}
   * @param key A string giving the name of the property to be retrieved.
   * @return the value of the property, or null if the property has not been defined.
   */
  def getUserData(key: String): AnyRef = {
    if (userData == null) {
      null
    } else {
      userData.get(key)
    }
  }
}

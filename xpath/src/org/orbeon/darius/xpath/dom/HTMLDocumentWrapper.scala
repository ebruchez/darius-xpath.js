// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.dom

import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.dom.HTMLDocumentWrapper._
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om.{Axis, DocumentInfo, NodeInfo}
import org.orbeon.darius.xpath.orbeon.{Configuration, HashMap}
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.scalajs.dom.{raw ⇒ dom}

import scala.util.control.Breaks

sealed trait DocType
case object DocTypeXHTML   extends DocType
case object DocTypeHTML    extends DocType
case object DocTypeUNKNOWN extends DocType
case object DocTypeNONHTML extends DocType

/**
 * The document node of a tree implemented as a wrapper around an XML DOM Document.
 */
class HTMLDocumentWrapper(
  doc        : dom.Node,
  baseURI    : String,
  config     : Configuration,
  newDocType : DocType
) extends HTMLNodeWrapper(
  doc,
  null,
  0
) with DocumentInfo {

  protected[dom] var _baseURI: String = _

  protected val documentNumber = config.allocateDocumentNumber()

  private var userData: HashMap[String, AnyRef] = _ //ORBEON could be Any, in which case we should change DocumentInfo

  private var idIndex: HashMap[String, HTMLNodeWrapper] = _

  private val isHttpRequested = newDocType == DocTypeNONHTML

  // XXX call a super constructor, this is ugly!
  nodeKind = Type.DOCUMENT
  docWrapper = this

  private var htmlType: DocType = DocTypeUNKNOWN

  locally {

    def nonEmptyOrNone(s: String) = Option(s) filterNot (_.isEmpty)

    _baseURI =
      nonEmptyOrNone(baseURI) match {
        case None if doc.nodeType == Type.DOCUMENT ⇒ // ORBEON: Q: In which case would it not be a document node?
          // MDN: "HTML documents have a document.URL property which returns the same value. Unlike URL, documentURI i
          // available on all types of documents." -> should we use document.URL when available?
          doc.asInstanceOf[dom.Document].documentURI
        case _ ⇒
          baseURI
      }

    //ORBEON TMP
    //this.htmlType = if (newDocType != DocTypeUNKNOWN) newDocType else findHTMLTypeFromRootElement
    this.htmlType = DocTypeHTML
  }

  //ORBEON: this requires that super has `nodeKind`, `node`, `docWrapper`
  private def findHTMLTypeFromRootElement: DocType = {
    try {
      val iter = this.iterateAxis(Axis.CHILD, NodeKindTest.ELEMENT)
      while (true) {//ORBEON: Mmh, there can only be 1 (and maybe 0) child element of a document, right?
        val elem = iter.next().asInstanceOf[NodeInfo]
        if (elem == null) {
          return null
        } else {
          val rawLocal = elem.asInstanceOf[HTMLNodeWrapper].getRawLocalName.toLowerCase
          if (rawLocal == "html") {
            val nb = elem.getDeclaredNamespaces(null)
            for (nBinding ← nb if nBinding.getURI == NamespaceConstant.XHTML) {
              return DocTypeXHTML
            }
            return DocTypeHTML
          } else {
            return DocTypeNONHTML
          }
        }
      }
    } catch {
      case e: Exception ⇒ //ORBEON: Q: NOP? What could it be?
    }
    null
  }

  def this(doc: dom.Node, baseURI: String, config: Configuration) =
    this(doc, baseURI, config, DocTypeUNKNOWN)

  def getDocType: DocType = htmlType

//  private def isNodeXMLDocument: Boolean = {
//    val nodeString = node.toString
//    if (nodeString.endsWith("XMLDocument]")) {
//      true
//    } else if (nodeString.endsWith("HTMLDocument]")) {
//      false
//    } else {
//      //ORBEON TODO test for existence of getElementById
//      false
//      //node.asInstanceOf[js.Any].getElementById
//    }
//  }

  /**
   * Create a wrapper for a node in this document
   *
   * @param node the DOM node to be wrapped. This must be a node within the document wrapped by this
   *             XMLDocumentWrapper
   * @throws IllegalArgumentException if the node is not a descendant of the Document node wrapped by
   *                                  this XMLDocumentWrapper
   */
  def wrap(node: dom.Node): HTMLNodeWrapper = {
    if (node == this.node) {
      return this
    }
    val doc = node.ownerDocument
    if (doc == this.node) {
      makeWrapper(node, this)
    } else {
      throw new IllegalArgumentException("XMLDocumentWrapper#wrap: supplied node does not belong to the wrapped DOM document")
    }
  }

  /**
   * Get the configuration previously set using setConfiguration
   */
  def getConfiguration: Configuration = config

  /**
   * Get the unique document number
   */
  override def getDocumentNumber: Int = documentNumber

  /**
   * Get the element with a given ID, if any
   *
   * @param id the required ID value
   * @return a NodeInfo representing the element with the given ID, or null if there
   *         is no such element. This relies on the getElementById() method in the
   *         underlying DOM.
   */
  def selectID(id: String): NodeInfo = {
    var el: dom.Node = null
    val doc = node.asInstanceOf[dom.Document]
    if (!isHttpRequested) {
      el = doc.getElementById(id)
      if (el == null) {
        return null
      }
      wrap(el)
    } else {
      if (idIndex != null) {
        idIndex.get(id)
      } else {
        idIndex = new HashMap[String, HTMLNodeWrapper]()
        val iter = iterateAxis(Axis.DESCENDANT, NodeKindTest.ELEMENT)
        val useNS = isNSok(node)
        import Breaks._
        breakable {
          while (true) {
            val node = iter.next().asInstanceOf[NodeInfo]
            if (node == null) {
              break()
            }
            val testNode = node.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode
            val xmlId = if (useNS) getXmlIdNS(testNode) else getXmlId(testNode)
            if (xmlId != null && ! xmlId.isEmpty) {
              idIndex.put(xmlId, node.asInstanceOf[HTMLNodeWrapper])
            }
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
  override def isSameNodeInfo(other: NodeInfo): Boolean = {
    other.isInstanceOf[HTMLDocumentWrapper] &&
      node == other.asInstanceOf[HTMLDocumentWrapper].node
  }

  /**
   * Set user data on the document node. The user data can be retrieved subsequently
   * using [[getUserData]]
   * @param key   A string giving the name of the property to be set. Clients are responsible
   *              for choosing a key that is likely to be unique. Must not be null.
   * @param value The value to be set for the property. May be null, which effectively
   *              removes the existing value for the property.
   */
  def setUserData(key: String, value: AnyRef): Unit = {
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
   * [[setUserData]]
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

private object HTMLDocumentWrapper {

  def getXmlIdNS(inNode: dom.Node): String =
    inNode.asInstanceOf[dom.Element].getAttributeNS("http://www.w3.org/XML/1998/namespace", "id")

  def getXmlId(inNode: dom.Node): String =
    inNode.asInstanceOf[dom.Element].getAttribute("xml:id")

  def isNSok(inNode: dom.Node): Boolean = {
    false
    //ORBEON TODO test existence of getAttributeNS?
//    inNode.asInstanceOf[dom.Element].getAttributeNS
  }
}
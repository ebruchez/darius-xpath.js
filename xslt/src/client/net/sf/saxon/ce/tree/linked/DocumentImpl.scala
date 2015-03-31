// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Builder
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.ListIterator
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.Whitespace
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A node in the XML parse tree representing the Document itself (or equivalently, the root
 * node of the Document).
 *
 * <p>A DocumentImpl object may either represent a real document node, or it may represent an imaginary
 * container for a parentless element.</p>
 * @author Michael H. Kay
 */
class DocumentImpl extends ParentNodeImpl with DocumentInfo {

  @BeanProperty
  var documentElement: ElementImpl = _

  private var idTable: HashMap[String, NodeInfo] = _

  @BeanProperty
  var documentNumber: Int = _

  private var baseURI: String = _

  private var elementList: HashMap[StructuredQName, List[NodeImpl]] = _

  private var userData: HashMap[String, Any] = _

  private var config: Configuration = _

  private var systemIdMap: SystemIdMap = new SystemIdMap()

  @BooleanBeanProperty
  var imaginary: Boolean = _

  setRawParent(null)

  /**
   * Set the Configuration that contains this document
   * @param config the Saxon configuration
   */
  def setConfiguration(config: Configuration): Unit = {
    this.config = config
    documentNumber = config.allocateDocumentNumber()
  }

  /**
   * Get the configuration previously set using setConfiguration
   * @return the Saxon configuration
   */
  def getConfiguration(): Configuration = config

  /**
   * Get a Builder suitable for building nodes that can be attached to this document.
   * @return a new TreeBuilder
   */
  def newBuilder(): Builder = {
    val builder = new LinkedTreeBuilder()
    builder.setAllocateSequenceNumbers(false)
    builder
  }

  /**
   * Set the system id (base URI) of this node
   */
  def setSystemId(uri: String): Unit = {
    if (uri == null) {
      uri = ""
    }
    systemIdMap.setSystemId(getRawSequenceNumber, uri)
  }

  /**
   * Get the system id of this root node
   */
  def getSystemId(): String = {
    systemIdMap.getSystemId(getRawSequenceNumber)
  }

  /**
   * Set the base URI of this document node
   * @param uri the new base URI
   */
  def setBaseURI(uri: String): Unit = {
    baseURI = uri
  }

  /**
   * Get the base URI of this root node.
   * @return the base URI
   */
  def getBaseURI(): String = {
    if (baseURI != null) {
      return baseURI
    }
    getSystemId
  }

  /**
   * Set the system id of an element in the document
   * @param seq the sequence number of the element
   * @param uri the system identifier (base URI) of the element
   */
  def setSystemId(seq: Int, uri: String): Unit = {
    if (uri == null) {
      uri = ""
    }
    systemIdMap.setSystemId(seq, uri)
  }

  /**
   * Get the system id of an element in the document
   * @param seq the sequence number of the element
   * @return the systemId (base URI) of the element
   */
  def getSystemId(seq: Int): String = systemIdMap.getSystemId(seq)

  /**
   * Return the type of node.
   * @return Type.DOCUMENT (always)
   */
  def getNodeKind(): Int = Type.DOCUMENT

  /**
   * Get next sibling - always null
   * @return null
   */
  def getNextSibling(): NodeInfo = null

  /**
   * Get previous sibling - always null
   * @return null
   */
  def getPreviousSibling(): NodeInfo = null

  /**
   * Get the root node
   * @return the NodeInfo representing the root of this tree
   */
  def getRoot(): NodeInfo = this

  /**
   * Get the root (document) node
   * @return the DocumentInfo representing this document
   */
  def getDocumentRoot(): DocumentInfo = this

  /**
   * Get the physical root of the tree. This may be an imaginary document node: this method
   * should be used only when control information held at the physical root is required
   * @return the document node, which may be imaginary
   */
  def getPhysicalRoot(): DocumentImpl = this

  /**
   * Get a character string that uniquely identifies this node
   *  @param buffer a buffer into which will be placed a string based on the document number
   *
   */
  def generateId(buffer: FastStringBuffer): Unit = {
    buffer.append('d')
    buffer.append(Long toString documentNumber)
  }

  /**
   * Get a list of all elements with a given name
   * @param name the fingerprint of the required element name
   * @return an iterator over all the elements with this name
   */
  def getAllElements(name: StructuredQName): UnfailingIterator = {
    if (elementList == null) {
      elementList = new HashMap[StructuredQName, List[NodeImpl]](100)
    }
    var list = elementList.get(name)
    if (list == null) {
      list = new ArrayList[NodeImpl](100)
      var next = getNextInDocument(this)
      while (next != null) {
        if (next.getNodeKind == Type.ELEMENT && next.getNodeName == name) {
          list.add(next)
        }
        next = next.getNextInDocument(this)
      }
      elementList.put(name, list)
    }
    new ListIterator(list)
  }

  /**
   * Index all the ID attributes. This is done the first time the id() function
   * is used on this document, or the first time that id() is called after a sequence of updates
   */
  private def indexIDs(): Unit = {
    if (idTable != null) {
      return
    }
    idTable = new HashMap[String, NodeInfo](256)
    var curr = this
    val root = curr
    while (curr != null) {
      if (curr.getNodeKind == Type.ELEMENT) {
        val e = curr.asInstanceOf[ElementImpl]
        val atts = e.getAttributeList
        for (i <- 0 until atts.getLength if atts.isId(i) && 
          NameChecker.isValidNCName(Whitespace.trim(atts.getValue(i)))) {
          registerID(e, Whitespace.trim(atts.getValue(i)))
        }
      }
      curr = curr.getNextInDocument(root)
    }
  }

  /**
   * Register a unique element ID. Does nothing if there is already an element with that ID.
   * @param e The Element having a particular unique ID value
   * @param id The unique ID value
   */
  protected def registerID(e: NodeInfo, id: String): Unit = {
    if (idTable == null) {
      idTable = new HashMap[String, NodeInfo](256)
    }
    val old = idTable.get(id)
    if (old == null) {
      idTable.put(id, e)
    }
  }

  /**
   * Get the element with a given ID.
   * @param id The unique ID of the required element, previously registered using registerID()
   * @return The NodeInfo for the given ID if one has been registered, otherwise null.
   */
  def selectID(id: String): NodeInfo = {
    if (idTable == null) {
      indexIDs()
    }
    idTable.get(id)
  }

  /**
   * Copy this node to a given outputter
   */
  def copy(out: Receiver, copyOptions: Int): Unit = {
    out.startDocument()
    var next = getFirstChild.asInstanceOf[NodeImpl]
    while (next != null) {
      next.copy(out, copyOptions)
      next = next.getNextSibling.asInstanceOf[NodeImpl]
    }
    out.endDocument()
  }

  /**
   * Set user data on the document node. The user data can be retrieved subsequently
   * using [[#getUserData]]
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
   * [[#setUserData]]
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

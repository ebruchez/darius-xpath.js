// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.linked

import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.tree.util.NamespaceIterator
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.Type
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ElementImpl implements an element with no attributes or namespace declarations.<P>
 * This class is an implementation of NodeInfo.
 * @author Michael H. Kay
 */
class ElementImpl extends ParentNodeImpl {

  private var elementName: StructuredQName = _

  private var attributeList: AttributeCollection = _

  @BeanProperty
  var namespaceList: Array[NamespaceBinding] = null

  /**
   * Set the name code. Used when creating a dummy element in the Stripper
   * @param name the new element name
   */
  def setNodeName(name: StructuredQName) {
    this.elementName = name
  }

  /**
   * Set the attribute list
   * @param atts the list of attributes of this element (not including namespace attributes)
   */
  def setAttributeList(atts: AttributeCollection) {
    this.attributeList = atts
  }

  /**
   * Initialise a new ElementImpl with an element name
   * @param qName the element name, with namespaces resolved
   * @param atts The attribute list: always null
   * @param parent  The parent node
   * @param sequenceNumber  Integer identifying this element within the document
   */
  def initialise(qName: StructuredQName, 
      atts: AttributeCollection, 
      parent: NodeInfo, 
      sequenceNumber: Int) {
    setNodeName(qName)
    setRawParent(parent.asInstanceOf[ParentNodeImpl])
    setRawSequenceNumber(sequenceNumber)
    attributeList = atts
  }

  /**
   * Set location information for this node
   * @param systemId the base URI
   */
  def setLocation(systemId: String) {
    val root = getRawParent.getPhysicalRoot
    root.setSystemId(getRawSequenceNumber, systemId)
  }

  /**
   * Set the system ID of this node. This method is provided so that a NodeInfo
   * implements the javax.xml.transform.Source interface, allowing a node to be
   * used directly as the Source of a transformation
   */
  def setSystemId(uri: String) {
    getPhysicalRoot.setSystemId(getRawSequenceNumber, uri)
  }

  /**
   * Get the root node
   */
  def getRoot(): NodeInfo = {
    val up = getRawParent
    if (up == null || 
      (up.isInstanceOf[DocumentImpl] && up.asInstanceOf[DocumentImpl].isImaginary)) {
      this
    } else {
      up.getRoot
    }
  }

  /**
   * Get the root node, if it is a document node.
   *
   * @return the DocumentInfo representing the containing document. If this
   *     node is part of a tree that does not have a document node as its
   *     root, returns null.
   * @since 8.4
   */
  def getDocumentRoot(): DocumentInfo = {
    val root = getRoot
    if (root.isInstanceOf[DocumentInfo]) {
      root.asInstanceOf[DocumentInfo]
    } else {
      null
    }
  }

  /**
   * Get the system ID of the entity containing this element node.
   */
  def getSystemId(): String = {
    val root = getPhysicalRoot
    (if (root == null) null else root.getSystemId(getRawSequenceNumber))
  }

  /**
   * Get the base URI of this element node. This will be the same as the System ID unless
   * xml:base has been used.
   */
  def getBaseURI(): String = Navigator.getBaseURI(this)

  def getNodeName(): StructuredQName = elementName

  /**
   * Get a character string that uniquely identifies this node
   * @param buffer to contain the generated ID
   */
  def generateId(buffer: FastStringBuffer) {
    val sequence = getRawSequenceNumber
    if (sequence >= 0) {
      getPhysicalRoot.generateId(buffer)
      buffer.append("e")
      buffer.append(Integer toString sequence)
    } else {
      getRawParent.generateId(buffer)
      buffer.append("f")
      buffer.append(Integer toString getSiblingPosition)
    }
  }

  /**
   * Return the kind of node.
   * @return Type.ELEMENT
   */
  def getNodeKind(): Int = Type.ELEMENT

  /**
   * Copy this node to a given outputter (supporting xsl:copy-of)
   * @param out The outputter
   * @param copyOptions
   */
  def copy(out: Receiver, copyOptions: Int) {
    out.startElement(getNodeName, 0)
    var childCopyOptions = copyOptions & ~CopyOptions.ALL_NAMESPACES
    if ((copyOptions & CopyOptions.LOCAL_NAMESPACES) != 0) {
      val localNamespaces = getDeclaredNamespaces(null)
      for (i <- 0 until localNamespaces.length) {
        val ns = localNamespaces(i)
        if (ns == null) {
          //break
        }
        out.namespace(ns, 0)
      }
    } else if ((copyOptions & CopyOptions.ALL_NAMESPACES) != 0) {
      NamespaceIterator.sendNamespaces(this, out)
      childCopyOptions |= CopyOptions.LOCAL_NAMESPACES
    }
    if (attributeList != null) {
      for (i <- 0 until attributeList.getLength) {
        val nc = attributeList.getStructuredQName(i)
        if (nc != null) {
          out.attribute(nc, attributeList.getValue(i))
        }
      }
    }
    out.startContent()
    var next = getFirstChild.asInstanceOf[NodeImpl]
    while (next != null) {
      next.copy(out, childCopyOptions)
      next = next.getNextSibling.asInstanceOf[NodeImpl]
    }
    out.endElement()
  }

  /**
   * Set the namespace declarations for the element
   * @param namespaces the list of namespace codes
   * @param namespacesUsed the number of entries in the list that are used
   */
  def setNamespaceDeclarations(namespaces: Array[NamespaceBinding], namespacesUsed: Int) {
    namespaceList = Array.ofDim[NamespaceBinding](namespacesUsed)
    System.arraycopy(namespaces, 0, namespaceList, 0, namespacesUsed)
  }

  /**
   * Search the NamespaceList for a given URI, returning the corresponding prefix.
   * @param uri The URI to be matched.
   * @return The prefix corresponding to this URI. If not found, return null. If there is
   * more than one prefix matching the URI, the first one found is returned. If the URI matches
   * the default namespace, return an empty string.
   */
  def getPrefixForURI(uri: String): String = {
    if (uri == NamespaceConstant.XML) {
      return "xml"
    }
    val iter = iterateAxis(Axis.NAMESPACE, AnyNodeTest.getInstance)
    while (true) {
      val ns = iter.next().asInstanceOf[NodeInfo]
      if (ns == null) {
        return null
      }
      if (ns.getStringValue == uri) {
        ns.getLocalPart
      }
    }
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
    (if (namespaceList == null) NamespaceBinding.EMPTY_ARRAY else namespaceList)
  }

  /**
   * Get the attribute list for this element.
   * @return The attribute list. This will not include any
   * namespace attributes. The attribute names will be in expanded form, with prefixes
   * replaced by URIs
   */
  def getAttributeList(): AttributeCollection = {
    (if (attributeList == null) AttributeCollection.EMPTY_ATTRIBUTE_COLLECTION else attributeList)
  }

  /**
   * Get the value of a given attribute of this node
   * @param uri the namespace URI of the attribute name, or "" if the attribute is not in a namepsace
   * @param localName the local part of the attribute name
   *  @return the attribute value if it exists or null if not
   */
  def getAttributeValue(uri: String, localName: String): String = {
    (if (attributeList == null) null else attributeList.getValue(uri, localName))
  }
}

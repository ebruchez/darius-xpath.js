// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.{Configuration, HashMap}
import client.net.sf.saxon.ce.pattern.NodeTest
import client.net.sf.saxon.ce.tree.iter.{ArrayIterator, EmptyIterator, SingletonIterator, UnfailingIterator}
import client.net.sf.saxon.ce.tree.util.{FastStringBuffer, Navigator, Orphan}

/**
 * This class represents a temporary tree whose root document node owns a single text node. <BR>
 */
class TextFragmentValue(value: CharSequence, baseURI: String) extends Orphan with DocumentInfo {

  private lazy val textNode = new TextFragmentTextNode(getStringValue, getSystemId)

  private var config: Configuration = _

  private var documentNumber: Int = _

  private var userData: HashMap[String, AnyRef] = _

  setNodeKind(Type.DOCUMENT)

  setNodeName(null)

  setStringValue(value)

  setSystemId(baseURI)

  /**
   * Set the configuration (containing the name pool used for all names in this document)
   */
  def setConfiguration(config: Configuration): Unit = {
    this.config = config
    documentNumber = -1
  }

  /**
   * Get the unique document number
   */
  override def getDocumentNumber(): Int = {
    if (documentNumber == -1) {
      documentNumber = config.allocateDocumentNumber()
    }
    documentNumber
  }

  /**
   * Determine whether this is the same node as another node
   * @return true if this Node object and the supplied Node object represent the
   * same node in the tree.
   */
  override def isSameNodeInfo(other: NodeInfo): Boolean = this == other

  /**
   * Determine the relative position of this node and another node, in document order.
   * The other node will always be in the same document.
   * @param other The other node, whose position is to be compared with this node
   * @return -1 if this node precedes the other node, +1 if it follows the other
   * node, or 0 if they are the same node. (In this case, isSameNode() will always
   * return true, and the two nodes will produce the same result for generateId())
   */
  override def compareOrder(other: NodeInfo): Int = {
    if (this == other) return 0
    -1
  }

  /**
   * Determine whether the node has any children.
   * @return <code>true</code> if this node has any attributes,
   *   <code>false</code> otherwise.
   */
  override def hasChildNodes(): Boolean = !("" == getStringValue)

  /**
   * Return an enumeration over the nodes reached by the given axis from this node
   * @param axisNumber The axis to be iterated over
   * @param nodeTest A pattern to be matched by the returned nodes
   * @return a AxisIterator that scans the nodes reached by the axis in turn.
   * @see client.net.sf.saxon.ce.om.Axis
   */
  override def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = axisNumber match {
    case Axis.ANCESTOR | Axis.ATTRIBUTE | Axis.FOLLOWING | Axis.FOLLOWING_SIBLING | Axis.NAMESPACE | Axis.PARENT | Axis.PRECEDING | Axis.PRECEDING_SIBLING ⇒ EmptyIterator.getInstance
    case Axis.SELF | Axis.ANCESTOR_OR_SELF ⇒ Navigator.filteredSingleton(this, nodeTest)
    case Axis.CHILD | Axis.DESCENDANT ⇒ Navigator.filteredSingleton(textNode, nodeTest)
    case Axis.DESCENDANT_OR_SELF ⇒
      var b1 = nodeTest.matchesItem(this)
      var textNode2 = textNode
      var b2 = nodeTest.matchesItem(textNode2)
      if (b1) {
        if (b2) {
          val pair = Array[Item](this, textNode2)
          new ArrayIterator(pair)
        } else {
          SingletonIterator.makeIterator(this)
        }
      } else {
        if (b2) {
          SingletonIterator.makeIterator(textNode2)
        } else {
          EmptyIterator.getInstance
        }
      }

    case _ ⇒ throw new IllegalArgumentException("Unknown axis number " + axisNumber)
  }

  /**
   * Get the root (document) node
   * @return the DocumentInfo representing the containing document
   */
  override def getDocumentRoot(): DocumentInfo = this

  /**
   * Copy the result tree fragment value to a given Outputter
   */
  override def copy(out: Receiver, copyOptions: Int): Unit = {
    out.characters(getStringValue)
  }

  /**
   * Get the element with a given ID.
   * @param id The unique ID of the required element
   * @return null (this kind of tree contains no elements)
   */
  def selectID(id: String): NodeInfo = null

  /**
   * Inner class representing the text node; this is created on demand
   */
  private class TextFragmentTextNode(value: CharSequence, systemId: String) extends Orphan with NodeInfo {

    setStringValue(value)

    setSystemId(systemId)

    setNodeKind(Type.TEXT)

    setNodeName(null)

    /**
     * Get a character string that uniquely identifies this node
     */
    override def generateId(buffer: FastStringBuffer): Unit = {
      getParent.generateId(buffer)
      buffer.append("t1")
    }

    /**
     * Determine the relative position of this node and another node, in document order.
     * The other node will always be in the same document.
     * @param other The other node, whose position is to be compared with this node
     * @return -1 if this node precedes the other node, +1 if it follows the other
     * node, or 0 if they are the same node. (In this case, isSameNode() will always
     * return true, and the two nodes will produce the same result for generateId())
     */
    override def compareOrder(other: NodeInfo): Int = {
      if (this == other) return 0
      +1
    }

    /**
     * Get the document number of the document containing this node. For a free-standing
     * orphan node, just return the hashcode.
     */
    override def getDocumentNumber(): Int = getDocumentRoot.getDocumentNumber

    /**
     * Return an enumeration over the nodes reached by the given axis from this node
     * @param axisNumber the axis to be iterated over
     * @param nodeTest A pattern to be matched by the returned nodes
     * @return a AxisIterator that scans the nodes reached by the axis in turn.
     */
    override def iterateAxis(axisNumber: Byte, nodeTest: NodeTest): UnfailingIterator = axisNumber match {
      case Axis.ANCESTOR | Axis.PARENT ⇒ Navigator.filteredSingleton(TextFragmentValue.this, nodeTest)
      case Axis.ANCESTOR_OR_SELF ⇒
        var matchesDoc = nodeTest.matchesItem(TextFragmentValue.this)
        var matchesText = nodeTest.matchesItem(this)
        if (matchesDoc && matchesText) {
          val nodes = Array[Item](this, TextFragmentValue.this)
          new ArrayIterator(nodes)
        } else if (matchesDoc && !matchesText) {
          SingletonIterator.makeIterator(TextFragmentValue.this)
        } else if (matchesText && !matchesDoc) {
          SingletonIterator.makeIterator(this)
        } else {
          EmptyIterator.getInstance
        }

      case Axis.ATTRIBUTE | Axis.CHILD | Axis.DESCENDANT | Axis.FOLLOWING | Axis.FOLLOWING_SIBLING | Axis.NAMESPACE | Axis.PRECEDING | Axis.PRECEDING_SIBLING ⇒ EmptyIterator.getInstance
      case Axis.SELF | Axis.DESCENDANT_OR_SELF ⇒ Navigator.filteredSingleton(this, nodeTest)
      case _ ⇒ throw new IllegalArgumentException("Unknown axis number " + axisNumber)
    }

    /**
     * Find the parent node of this node.
     * @return The Node object describing the containing element or root node.
     */
    override def getParent(): NodeInfo = TextFragmentValue.this

    /**
     * Get the root node
     * @return the NodeInfo representing the root of this tree
     */
    override def getRoot(): NodeInfo = TextFragmentValue.this

    /**
     * Get the root (document) node
     * @return the DocumentInfo representing the containing document
     */
    override def getDocumentRoot(): DocumentInfo = TextFragmentValue.this

    /**
     * Copy the node to a given Outputter
     */
    override def copy(out: Receiver, copyOptions: Int): Unit = {
      out.characters(getStringValue)
    }
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

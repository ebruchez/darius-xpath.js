// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

object AttributeCollection {

  var EMPTY_ATTRIBUTE_COLLECTION: AttributeCollection = new AttributeCollection()
}

/**
 * AttributeCollection represents the collection of attributes available on a particular element
 * node. It is modelled on the SAX2 Attributes interface, but is extended firstly to work with
 * Saxon NamePools, and secondly to provide type information as required by the XPath 2.0 data model.
 */
class AttributeCollection {

  private var values: Array[String] = null

  private var names: Array[StructuredQName] = null

  private var used: Int = 0

  /**
   * Add an attribute to an attribute list. The parameters correspond
   * to the parameters of the [[client.net.sf.saxon.ce.event.Receiver.attribute(StructuredQName, CharSequence)]]
   * method. There is no check that the name of the attribute is distinct from other attributes
   * already in the collection: this check must be made by the caller.
   *
   * @param nameCode Integer representing the attribute name.
   * @param value    The attribute value (must not be null)
   */
  def addAttribute(nameCode: StructuredQName, value: String): Unit = {
    if (values == null) {
      values = new Array[String](5)
      names = new Array[StructuredQName](5)
      used = 0
    }
    if (values.length == used) {
      val newsize = if (used == 0) 5 else used * 2
      val v2 = new Array[String](newsize)
      val c2 = new Array[StructuredQName](newsize)
      System.arraycopy(values, 0, v2, 0, used)
      System.arraycopy(names, 0, c2, 0, used)
      values = v2
      names = c2
    }
    names(used) = nameCode
    values(used) = value
    used += 1
  }

  /**
   * Clear the attribute list. This removes the values but doesn't free the memory used.
   * free the memory, use clear() then compact().
   */
  def clear(): Unit = {
    used = 0
  }

  /**
   * Compact the attribute list to avoid wasting memory
   */
  def compact(): Unit = {
    if (used == 0) {
      names = null
      values = null
    } else if (values.length > used) {
      val v2 = new Array[String](used)
      val c2 = new Array[StructuredQName](used)
      System.arraycopy(values, 0, v2, 0, used)
      System.arraycopy(names, 0, c2, 0, used)
      values = v2
      names = c2
    }
  }

  /**
   * Return the number of attributes in the list.
   *
   * @return The number of attributes that have been created in this attribute collection. This is the number
   * of slots used in the list, including any slots allocated to attributes that have since been deleted.
   * Such slots are not reused, to preserve attribute identity.
   */
  def getLength(): Int = if (values == null) 0 else used

  /**
   * Get the name of an attribute (by position).
   *
   * @param index The position of the attribute in the list.
   * @return The  name of the attribute as a StructuredQName, or null if there
   *         is no attribute at that position.
   */
  def getStructuredQName(index: Int): StructuredQName = {
    if (names == null) {
      return null
    }
    if (index < 0 || index >= used) {
      return null
    }
    names(index)
  }

  /**
   * Get the prefix of the name of an attribute (by position).
   *
   * @param index The position of the attribute in the list.
   * @return The prefix of the attribute name as a string, or null if there
   *         is no attribute at that position. Returns "" for an attribute that
   *         has no prefix.
   */
  def getPrefix(index: Int): String = {
    if (names == null) {
      return null
    }
    if (index < 0 || index >= used) {
      return null
    }
    names(index).getPrefix
  }

  /**
   * Get the local name of an attribute (by position).
   *
   * @param index The position of the attribute in the list.
   * @return The local name of the attribute as a string, or null if there
   *         is no attribute at that position.
   */
  def getLocalName(index: Int): String = {
    if (names == null) {
      return null
    }
    if (index < 0 || index >= used) {
      return null
    }
    names(index).getLocalName
  }

  /**
   * Get the namespace URI of an attribute (by position).
   *
   * @param index The position of the attribute in the list.
   * @return The local name of the attribute as a string, or null if there
   *         is no attribute at that position.
   */
  def getURI(index: Int): String = {
    if (names == null) {
      return null
    }
    if (index < 0 || index >= used) {
      return null
    }
    names(index).getNamespaceURI
  }

  /**
   * Get the value of an attribute (by position).
   *
   * @param index The position of the attribute in the list.
   * @return The attribute value as a string, or null if
   *         there is no attribute at that position.
   */
  def getValue(index: Int): String = {
    if (values == null) {
      return null
    }
    if (index < 0 || index >= used) {
      return null
    }
    values(index)
  }

  /**
   * Get the value of an attribute (by name).
   *
   * @param uri       The namespace uri of the attribute.
   * @param localname The local name of the attribute.
   * @return The index position of the attribute
   */
  def getValue(uri: String, localname: String): String = {
    if (names == null) {
      return null
    }
    for (i ← 0 until used if names(i).getNamespaceURI == uri && names(i).getLocalName == localname) {
      return values(i)
    }
    null
  }

  /**
   * Find an attribute by structured QName
   * @param name the fingerprint representing the name of the required attribute
   * @return the index of the attribute, or -1 if absent
   */
  def findByStructuredQName(name: StructuredQName): Int = {
    if (names == null) {
      return -1
    }
    for (i ← 0 until used if names(i) == name) {
      return i
    }
    -1
  }

  /**
   * Determine whether a given attribute has the is-ID property set
   */
  def isId(index: Int): Boolean = StructuredQName.XML_ID == names(index)
}

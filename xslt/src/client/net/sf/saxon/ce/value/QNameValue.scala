// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.functions.Component
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`._
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A QName value. This implements the so-called "triples proposal", in which the prefix is retained as
 * part of the value. The prefix is not used in any operation on a QName other than conversion of the
 * QName to a string.
 */
class QNameValue(prefix: String, uri: String, localName: String) extends AtomicValue {

  protected var qName: StructuredQName = new StructuredQName(prefix, uri, localName)

  /**
   * Constructor starting from a StructuredQName
   * @param name the QName
   */
  def this(name: StructuredQName) {
    this()
    qName = name
  }

  /**
   * Constructor. This constructor validates that the local part is a valid NCName.
   *
   * @param prefix The prefix part of the QName (not used in comparisons). Use "" to represent the
   * default prefix (but null is also accepted).
   * Note that the prefix is not checked for lexical correctness, because in most cases
   * it will already have been matched against in-scope namespaces. Where necessary the caller must
   * check the prefix.
   * @param uri The namespace part of the QName. Use null to represent the non-namespace (but "" is also
   * accepted).
   * @param localName The local part of the QName
   * @throws XPathException if the local part of the name is malformed or if the name has a null
   * namespace with a non-empty prefix
   */
  def this(prefix: String, 
      uri: String, 
      localName: String, 
      validate: Boolean) {
    this()
    if (!NameChecker.isValidNCName(localName)) {
      val err = new XPathException("Malformed local name in QName: '" + localName + '\'')
      err.setErrorCode("FORG0001")
      throw err
    }
    prefix = if (prefix == null) "" else prefix
    uri = if ("" == uri) null else uri
    if (uri == null && prefix.length != 0) {
      val err = new XPathException("QName has null namespace but non-empty prefix")
      err.setErrorCode("FOCA0002")
      throw err
    }
    qName = new StructuredQName(prefix, uri, localName)
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType: AtomicType = AtomicType.QNAME

  /**
   * Convert a QName to target data type
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.QNAME) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else {
      new ValidationFailure("Cannot convert QName to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Get a component. Returns a zero-length string if the namespace-uri component is
   * requested and is not present.
   * @param part either Component.LOCALNAME or Component.NAMESPACE indicating which
   * component of the value is required
   * @return either the local name or the namespace URI, in each case as a StringValue
   */
  def getComponent(part: Int): AtomicValue = {
    if (part == Component.LOCALNAME) {
      new StringValue(getLocalName)
    } else if (part == Component.NAMESPACE) {
      new AnyURIValue(getNamespaceURI)
    } else if (part == Component.PREFIX) {
      val prefix = getPrefix
      if (prefix.length == 0) {
        null
      } else {
        new StringValue(prefix)
      }
    } else {
      throw new UnsupportedOperationException("Component of QName must be URI, Local Name, or Prefix")
    }
  }

  /**
   * Get the string value as a String. Returns the QName as a lexical QName, retaining the original
   * prefix if available.
   */
  def getPrimitiveStringValue: String = qName.getDisplayName

  /**
   * Convert to a StructuredQName
   * @return the name as a StructuredQName
   */
  def toStructuredQName(): StructuredQName = qName

  /**
   * Get the QName in Clark notation, that is "{uri}local" if in a namespace, or "local" otherwise
   */
  def getClarkName(): String = qName.getClarkName

  /**
   * Get the local part
   */
  def getLocalName(): String = qName.getLocalName

  /**
   * Get the namespace part. Returns the empty string for a name in no namespace.
   */
  def getNamespaceURI(): String = qName.getNamespaceURI

  /**
   * Get the prefix. Returns the empty string if the name is unprefixed.
   */
  def getPrefix(): String = qName.getPrefix

  /**
   * Get an object value that implements the XPath equality and ordering comparison semantics for this value.
   * If the ordered parameter is set to true, the result will be a Comparable and will support a compareTo()
   * method with the semantics of the XPath lt/gt operator, provided that the other operand is also obtained
   * using the getXPathComparable() method. In all cases the result will support equals() and hashCode() methods
   * that support the semantics of the XPath eq operator, again provided that the other operand is also obtained
   * using the getXPathComparable() method. A context argument is supplied for use in cases where the comparison
   * semantics are context-sensitive, for example where they depend on the implicit timezone or the default
   * collation.
   *
   * @param ordered true if an ordered comparison is required. In this case the result is null if the
   *                type is unordered; in other cases the returned value will be a Comparable.
   * @param collator
   * @param implicitTimezone
   */
  def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    if (ordered) null else this
  }

  override def hashCode(): Int = qName.hashCode

  /**
   * The toString() method returns the name in the form QName("uri", "local")
   * @return the name in in the form QName("uri", "local")
   */
  override def toString(): String = {
    "QName(\"" + getNamespaceURI + "\", \"" + getLocalName + 
      "\")"
  }

  /**
   * Determine if two QName values are equal. This comparison ignores the prefix part
   * of the value.
   * @throws ClassCastException if they are not comparable
   */
  override def equals(other: Any): Boolean = other match {
    case other: QNameValue ⇒ qName == other.qName
    case _ ⇒ false
  }
}

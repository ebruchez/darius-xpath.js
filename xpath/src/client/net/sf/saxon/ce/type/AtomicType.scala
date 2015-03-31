// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.`type`

import client.net.sf.saxon.ce.`type`.AtomicType._
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.HashMap
import client.net.sf.saxon.ce.value._

object AtomicType {

  private val lookup: HashMap[String, AtomicType] = new HashMap[String, AtomicType](20)

  /**
   * Internal factory method to create a BuiltInAtomicType. There is one instance for each of the
   * built-in atomic types
   *
   * @param localName The name of the type within the XSD namespace
   * @param baseType    The base type from which this type is derived
   * @param ordered True if an ordering relationship is defined for this type
   * @return the newly constructed built in atomic type
   */
  private def makeAtomicType(localName: String, baseType: ItemType, ordered: Boolean): AtomicType = {
    val t = new AtomicType(localName)
    t.baseType = baseType
    t.ordered = ordered
    register(localName, t)
    t
  }

  def isRecognizedName(localName: String): Boolean = {
    getSchemaType(localName) != null || "anyType" == localName || 
      "untyped" == localName || 
      "anySimpleType" == localName
  }

  /**
   * Get the schema type with a given fingerprint
   * @param localName the local name of the type, in the XSD namespace
   * @return the SchemaType object representing the given type, if known, otherwise null
   */
  def getSchemaType(localName: String): AtomicType = {
    var st = lookup.get(localName)
    if (st == null) {
      if (AtomicType.DOUBLE == null) {
      }
      st = lookup.get(localName)
    }
    st
  }

  /**
   * Method for internal use to register a built in type with this class
   * @param localName the type name within the XSD namespace
   * @param type the SchemaType representing the built in type
   */
  def register(localName: String, `type`: AtomicType): Unit = {
    lookup.put(localName, `type`)
  }

  var ANY_ATOMIC: AtomicType = makeAtomicType("anyAtomicType", AnyItemType.getInstance, true)

  var NUMERIC: AtomicType = makeAtomicType("numeric", ANY_ATOMIC, true)

  var STRING: AtomicType = makeAtomicType("string", ANY_ATOMIC, true)

  var BOOLEAN: AtomicType = makeAtomicType("boolean", ANY_ATOMIC, true)

  var DURATION: AtomicType = makeAtomicType("duration", ANY_ATOMIC, false)

  var DATE_TIME: AtomicType = makeAtomicType("dateTime", ANY_ATOMIC, true)

  var DATE: AtomicType = makeAtomicType("date", ANY_ATOMIC, true)

  var TIME: AtomicType = makeAtomicType("time", ANY_ATOMIC, true)

  var G_YEAR_MONTH: AtomicType = makeAtomicType("gYearMonth", ANY_ATOMIC, false)

  var G_MONTH: AtomicType = makeAtomicType("gMonth", ANY_ATOMIC, false)

  var G_MONTH_DAY: AtomicType = makeAtomicType("gMonthDay", ANY_ATOMIC, false)

  var G_YEAR: AtomicType = makeAtomicType("gYear", ANY_ATOMIC, false)

  var G_DAY: AtomicType = makeAtomicType("gDay", ANY_ATOMIC, false)

  var HEX_BINARY: AtomicType = makeAtomicType("hexBinary", ANY_ATOMIC, false)

  var BASE64_BINARY: AtomicType = makeAtomicType("base64Binary", ANY_ATOMIC, false)

  var ANY_URI: AtomicType = makeAtomicType("anyURI", ANY_ATOMIC, true)

  var QNAME: AtomicType = makeAtomicType("QName", ANY_ATOMIC, false)

  var UNTYPED_ATOMIC: AtomicType = makeAtomicType("untypedAtomic", ANY_ATOMIC, true)

  var DECIMAL: AtomicType = makeAtomicType("decimal", NUMERIC, true)

  var FLOAT: AtomicType = makeAtomicType("float", NUMERIC, true)

  var DOUBLE: AtomicType = makeAtomicType("double", NUMERIC, true)

  var INTEGER: AtomicType = makeAtomicType("integer", DECIMAL, true)

  var YEAR_MONTH_DURATION: AtomicType = makeAtomicType("yearMonthDuration", DURATION, true)

  var DAY_TIME_DURATION: AtomicType = makeAtomicType("dayTimeDuration", DURATION, true)
}

/**
 * This class represents a built-in atomic type, which may be either a primitive type
 * (such as xs:decimal or xs:anyURI) or a derived type (such as xs:ID or xs:dayTimeDuration).
 */
class AtomicType private (var localName: String) extends ItemType {

  var baseType: ItemType = _

  var ordered: Boolean = false

  /**
   * Determine whether the atomic type is ordered, that is, whether less-than and greater-than comparisons
   * are permitted
   *
   * @return true if ordering operations are permitted
   */
  def isOrdered: Boolean = ordered

  /**
   * Determine whether the atomic type is numeric
   *
   * @return true if the type is a built-in numeric type
   */
  def isPrimitiveNumeric: Boolean = {
    this == NUMERIC || this == INTEGER || baseType == NUMERIC
  }

  /**
   * Get the display name of the type: that is, a lexical QName with an arbitrary prefix
   *
   * @return a lexical QName identifying the type
   */
  def getDisplayName: String = "xs:" + localName

  /**
   * Test whether a given item conforms to this type
   * @param item  The item to be tested
   * @return true if the item is an instance of this type; false otherwise
   */
  def matchesItem(item: Item): Boolean = {
    item match {
      case value: AtomicValue ⇒
        var `type`: ItemType = value.getItemType
        do {
          if (`type` == this) {
            return true
          }
          `type` = `type`.getSuperType
        } while (`type` != null);
      case _ ⇒
    }
    false
  }

  /**
   * Get the type from which this item type is derived by restriction. This
   * is the supertype in the XPath type heirarchy, as distinct from the Schema
   * base type: this means that the supertype of xs:boolean is xs:anyAtomicType,
   * whose supertype is item() (rather than xs:anySimpleType).
   *
   * @return the supertype, or null if this type is item()
   */
  def getSuperType: ItemType = baseType

  /**
   * Get the item type of the atomic values that will be produced when an item
   * of this type is atomized
   */
  def getAtomizedItemType: AtomicType = this

  override def toString: String = getDisplayName
}

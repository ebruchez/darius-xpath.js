// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.`type`

import java.{util ⇒ ju}

import org.orbeon.darius.xpath.`type`.AtomicType._
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.value._

object AtomicType {

  private val lookup: ju.HashMap[String, AtomicType] = new ju.HashMap[String, AtomicType](20)

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

  var ANY_ATOMIC: AtomicType = makeAtomicType("anyAtomicType", AnyItemType.getInstance, ordered = true)

  var NUMERIC: AtomicType = makeAtomicType("numeric", ANY_ATOMIC, ordered = true)

  var STRING: AtomicType = makeAtomicType("string", ANY_ATOMIC, ordered = true)

  var BOOLEAN: AtomicType = makeAtomicType("boolean", ANY_ATOMIC, ordered = true)

  var DURATION: AtomicType = makeAtomicType("duration", ANY_ATOMIC, ordered = false)

  var DATE_TIME: AtomicType = makeAtomicType("dateTime", ANY_ATOMIC, ordered = true)

  var DATE: AtomicType = makeAtomicType("date", ANY_ATOMIC, ordered = true)

  var TIME: AtomicType = makeAtomicType("time", ANY_ATOMIC, ordered = true)

  var G_YEAR_MONTH: AtomicType = makeAtomicType("gYearMonth", ANY_ATOMIC, ordered = false)

  var G_MONTH: AtomicType = makeAtomicType("gMonth", ANY_ATOMIC, ordered = false)

  var G_MONTH_DAY: AtomicType = makeAtomicType("gMonthDay", ANY_ATOMIC, ordered = false)

  var G_YEAR: AtomicType = makeAtomicType("gYear", ANY_ATOMIC, ordered = false)

  var G_DAY: AtomicType = makeAtomicType("gDay", ANY_ATOMIC, ordered = false)

  var HEX_BINARY: AtomicType = makeAtomicType("hexBinary", ANY_ATOMIC, ordered = false)

  var BASE64_BINARY: AtomicType = makeAtomicType("base64Binary", ANY_ATOMIC, ordered = false)

  var ANY_URI: AtomicType = makeAtomicType("anyURI", ANY_ATOMIC, ordered = true)

  var QNAME: AtomicType = makeAtomicType("QName", ANY_ATOMIC, ordered = false)

  var UNTYPED_ATOMIC: AtomicType = makeAtomicType("untypedAtomic", ANY_ATOMIC, ordered = true)

  var DECIMAL: AtomicType = makeAtomicType("decimal", NUMERIC, ordered = true)

  var FLOAT: AtomicType = makeAtomicType("float", NUMERIC, ordered = true)

  var DOUBLE: AtomicType = makeAtomicType("double", NUMERIC, ordered = true)

  var INTEGER: AtomicType = makeAtomicType("integer", DECIMAL, ordered = true)

  var YEAR_MONTH_DURATION: AtomicType = makeAtomicType("yearMonthDuration", DURATION, ordered = true)

  var DAY_TIME_DURATION: AtomicType = makeAtomicType("dayTimeDuration", DURATION, ordered = true)
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
   * is the supertype in the XPath type hierarchy, as distinct from the Schema
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

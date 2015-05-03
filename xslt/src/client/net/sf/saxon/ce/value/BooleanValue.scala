// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.trans.Err
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.AtomicType
import org.orbeon.darius.xpath.`type`.ConversionResult
import org.orbeon.darius.xpath.`type`.ValidationFailure
import BooleanValue._
//remove if not needed
import scala.collection.JavaConversions._

object BooleanValue {

  /**
   * The boolean value TRUE
   */
  val TRUE = new BooleanValue(true)

  /**
   * The boolean value FALSE
   */
  val FALSE = new BooleanValue(false)

  /**
   * Factory method: get a BooleanValue
   *
   * @param value true or false, to determine which boolean value is
   *     required
   * @return the BooleanValue requested
   */
  def get(value: Boolean): BooleanValue = if (value) TRUE else FALSE

  /**
   * Convert a string to a boolean value, using the XML Schema rules (including
   * whitespace trimming)
   * @param s the input string
   * @return the relevant BooleanValue if validation succeeds; or a ValidationFailure if not.
   */
  def fromString(s: CharSequence): ConversionResult = {
    s = Whitespace.trimWhitespace(s)
    val len = s.length
    if (len == 1) {
      val c = s.charAt(0)
      if (c == '1') {
        return TRUE
      } else if (c == '0') {
        return FALSE
      }
    } else if (len == 4) {
      if (s.charAt(0) == 't' && s.charAt(1) == 'r' && s.charAt(2) == 'u' && 
        s.charAt(3) == 'e') {
        return TRUE
      }
    } else if (len == 5) {
      if (s.charAt(0) == 'f' && s.charAt(1) == 'a' && s.charAt(2) == 'l' && 
        s.charAt(3) == 's' && 
        s.charAt(4) == 'e') {
        return FALSE
      }
    }
    new ValidationFailure("The string " + Err.wrap(s, Err.VALUE) + " cannot be cast to a boolean", "FORG0001")
  }
}

/**
 * A boolean XPath value
 */
class BooleanValue private (var value: Boolean) extends AtomicValue with Comparable[_] {

  /**
   * Get the value
   * @return true or false, the actual boolean value of this BooleanValue
   */
  def getBooleanValue(): Boolean = value

  /**
   * Get the effective boolean value of this expression
   *
   * @return the boolean value
   */
  def effectiveBooleanValue(): Boolean = value

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType: AtomicType = AtomicType.BOOLEAN

  /**
   * Convert to target data type
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.BOOLEAN) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      if (value) StringValue.TRUE else StringValue.FALSE
    } else if (requiredType == AtomicType.NUMERIC || requiredType == AtomicType.INTEGER || 
      requiredType == AtomicType.DECIMAL) {
      if (value) IntegerValue.PLUS_ONE else IntegerValue.ZERO
    } else if (requiredType == AtomicType.DOUBLE) {
      if (value) DoubleValue.ONE else DoubleValue.ZERO
    } else if (requiredType == AtomicType.FLOAT) {
      if (value) FloatValue.ONE else FloatValue.ZERO
    } else {
      new ValidationFailure("Cannot convert boolean to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert to string
   * @return "true" or "false"
   */
  def getPrimitiveStringValue: String = if (value) "true" else "false"

  /**
   * Get a Comparable value that implements the XPath ordering comparison semantics for this value.
   * Returns null if the value is not comparable according to XPath rules. The default implementation
   * returns null. This is overridden for types that allow ordered comparisons in XPath: numeric, boolean,
   * string, date, time, dateTime, yearMonthDuration, dayTimeDuration, and anyURI.
   * @param ordered
   * @param collator
   * @param implicitTimezone
   */
  def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    this
  }

  /**
   * Compare the value to another boolean value
   *
   * @throws ClassCastException if the other value is not a BooleanValue
   *     (the parameter is declared as Object to satisfy the Comparable
   *     interface)
   * @param other The other boolean value
   * @return -1 if this one is the lower, 0 if they are equal, +1 if this
   *     one is the higher. False is considered to be less than true.
   */
  def compareTo(other: AnyRef): Int = {
    if (!other.isInstanceOf[BooleanValue]) {
      throw new ClassCastException("Boolean values are not comparable to " + other.getClass)
    }
    if (value == other.asInstanceOf[BooleanValue].value) return 0
    if (value) return +1
    -1
  }

  /**
   * Determine whether two boolean values are equal
   *
   * @param other the value to be compared to this value
   * @return true if the other value is a boolean value and is equal to this
   *      value
   * @throws ClassCastException if other value is not xs:boolean or derived therefrom
   */
  override def equals(other: Any): Boolean = {
    other.isInstanceOf[BooleanValue] && value == other.asInstanceOf[BooleanValue].value
  }

  /**
   * Get a hash code for comparing two BooleanValues
   *
   * @return the hash code
   */
  override def hashCode(): Int = if (value) 0 else 1

  /**
   * Diagnostic display of this value as a string
   * @return a string representation of this value: "true()" or "false()"
   */
  override def toString: String = getStringValue + "()"
}

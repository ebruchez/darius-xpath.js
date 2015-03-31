// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.`type`.{AtomicType, ConversionResult, ValidationFailure}
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer

private object HexBinaryValue {
  /**
   * Decode a single hex digit
   *
   * @param c the hex digit
   * @return the numeric value of the hex digit
   * @throws XPathException if it isn't a hex digit
   */
  def fromHex(c: Char): Int = {
    var d = "0123456789ABCDEFabcdef".indexOf(c)
    if (d > 15) {
      d = d - 6
    }
    if (d < 0) {
      val err = new XPathException("Invalid hexadecimal digit")
      err.setErrorCode("FORG0001")
      throw err
    }
    d
  }
}

/**
 * A value of type xs:hexBinary
 */
class HexBinaryValue(private val binaryValue: Array[Byte]) extends AtomicValue {

  def this(in: CharSequence) =
    this(
      {
        val s = Whitespace.trimWhitespace(in)
        val binaryValue = new Array[Byte](s.length / 2)

        if ((s.length & 1) != 0) {
          val err = new XPathException("A hexBinary value must contain an even number of characters")
          err.setErrorCode("FORG0001")
          throw err
        }

        for (i <- 0 until binaryValue.length) {
          binaryValue(i) = ((HexBinaryValue.fromHex(s.charAt(2 * i)) << 4) + (HexBinaryValue.fromHex(s.charAt(2 * i + 1)))).toByte
        }

        binaryValue
      }
    )

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.HEX_BINARY

  /**
   * Convert to target data type
   *
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.HEX_BINARY) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.BASE64_BINARY) {
      new Base64BinaryValue(binaryValue)
    } else {
      new ValidationFailure("Cannot convert gYearMonth to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert to string
   *
   * @return the canonical representation.
   */
  def getPrimitiveStringValue(): CharSequence = {
    val digits = "0123456789ABCDEF"
    val sb = new FastStringBuffer(binaryValue.length * 2)
    for (i <- 0 until binaryValue.length) {
      sb.append(digits.charAt((binaryValue(i) >> 4) & 0xf))
      sb.append(digits.charAt(binaryValue(i) & 0xf))
    }
    sb
  }

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
    (if (ordered) null else this)
  }

  /**
   * Test if the two hexBinary or Base64Binaryvalues are equal.
   */
  override def equals(other: Any): Boolean = other match {
    case other: HexBinaryValue => binaryValue.sameElements(other.binaryValue)
    case _ => false
  }

  override def hashCode(): Int = {
    Base64BinaryValue.byteArrayHashCode(binaryValue)
  }
}

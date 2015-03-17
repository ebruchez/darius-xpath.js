// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.tree.util.UTF16CharacterSet
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.StringToDouble
import client.net.sf.saxon.ce.`type`.ValidationFailure
import StringValue._
//remove if not needed
import scala.collection.JavaConversions._

object StringValue {

  val EMPTY_STRING = new StringValue("")

  val SINGLE_SPACE = new StringValue(" ")

  val TRUE = new StringValue("true")

  val FALSE = new StringValue("false")

  /**
   * Factory method. Unlike the constructor, this avoids creating a new StringValue in the case
   * of a zero-length string (and potentially other strings, in future)
   *
   * @param value the String value. Null is taken as equivalent to "".
   * @return the corresponding StringValue
   */
  def makeStringValue(value: CharSequence): StringValue = {
    if (value == null || value.length == 0) {
      StringValue.EMPTY_STRING
    } else {
      new StringValue(value)
    }
  }

  /**
   * Convert a string value to another built-in data type, with control over how validation is
   * handled.
   *
   * @param value        the value to be converted
   * @param requiredType the required atomic type. This must not be a namespace-sensitive type.
   * @return the result of the conversion, if successful. If unsuccessful, the value returned
   *         will be a [[ValidationFailure]]. The caller must check for this condition. No exception is thrown, instead
   *         the exception will be encapsulated within the ValidationFailure.
   */
  def convertStringToBuiltInType(value: CharSequence, requiredType: AtomicType): ConversionResult = {
    try {
      if (requiredType == AtomicType.BOOLEAN) {
        BooleanValue.fromString(value)
      } else if (requiredType == AtomicType.NUMERIC || requiredType == AtomicType.DOUBLE) {
        try {
          val dbl = StringToDouble.stringToNumber(value)
          new DoubleValue(dbl)
        } catch {
          case err: NumberFormatException => new ValidationFailure("Cannot convert string to double: " + value.toString, 
            "FORG0001")
        }
      } else if (requiredType == AtomicType.INTEGER) {
        IntegerValue.stringToInteger(value)
      } else if (requiredType == AtomicType.DECIMAL) {
        DecimalValue.makeDecimalValue(value)
      } else if (requiredType == AtomicType.FLOAT) {
        try {
          val flt = StringToDouble.stringToNumber(value).toFloat
          new FloatValue(flt)
        } catch {
          case err: NumberFormatException => new ValidationFailure("Cannot convert string to float: " + value.toString, 
            "FORG0001")
        }
      } else if (requiredType == AtomicType.DATE) {
        DateValue.makeDateValue(value)
      } else if (requiredType == AtomicType.DATE_TIME) {
        DateTimeValue.makeDateTimeValue(value)
      } else if (requiredType == AtomicType.TIME) {
        TimeValue.makeTimeValue(value)
      } else if (requiredType == AtomicType.G_YEAR) {
        GYearValue.makeGYearValue(value)
      } else if (requiredType == AtomicType.G_YEAR_MONTH) {
        GYearMonthValue.makeGYearMonthValue(value)
      } else if (requiredType == AtomicType.G_MONTH) {
        GMonthValue.makeGMonthValue(value)
      } else if (requiredType == AtomicType.G_MONTH_DAY) {
        GMonthDayValue.makeGMonthDayValue(value)
      } else if (requiredType == AtomicType.G_DAY) {
        GDayValue.makeGDayValue(value)
      } else if (requiredType == AtomicType.DURATION) {
        DurationValue.makeDuration(value)
      } else if (requiredType == AtomicType.YEAR_MONTH_DURATION) {
        YearMonthDurationValue.makeYearMonthDurationValue(value)
      } else if (requiredType == AtomicType.DAY_TIME_DURATION) {
        DayTimeDurationValue.makeDayTimeDurationValue(value)
      } else if (requiredType == AtomicType.UNTYPED_ATOMIC || requiredType == AtomicType.ANY_ATOMIC) {
        new UntypedAtomicValue(value)
      } else if (requiredType == AtomicType.STRING) {
        makeStringValue(value)
      } else if (requiredType == AtomicType.ANY_URI) {
        new AnyURIValue(value)
      } else if (requiredType == AtomicType.HEX_BINARY) {
        new HexBinaryValue(value)
      } else if (requiredType == AtomicType.BASE64_BINARY) {
        new Base64BinaryValue(value)
      } else {
        new ValidationFailure("Cannot convert string to type " + Err.wrap(requiredType.getDisplayName), 
          "XPTY0004")
      }
    } catch {
      case err: XPathException => {
        val vf = new ValidationFailure(err.getMessage)
        vf.setErrorCodeQName(err.getErrorCodeQName)
        if (vf.getErrorCodeQName == null) {
          vf.setErrorCode("FORG0001")
        }
        vf
      }
    }
  }

  /**
   * Get the length of a string, as defined in XPath. This is not the same as the Java length,
   * as a Unicode surrogate pair counts as a single character.
   *
   * @param s The string whose length is required
   * @return the length of the string in Unicode code points
   */
  def getStringLength(s: CharSequence): Int = {
    var n = 0
    for (i <- 0 until s.length) {
      val c = s.charAt(i).toInt
      if (c < 55296 || c > 56319) n += 1
    }
    n
  }

  /**
   * Expand a string containing surrogate pairs into an array of 32-bit characters
   *
   * @param s the string to be expanded
   * @return an array of integers representing the Unicode code points
   */
  def expand(s: CharSequence): Array[Int] = {
    val array = Array.ofDim[Int](getStringLength(s))
    val o = 0
    for (i <- 0 until s.length) {
      var charval: Int = 0
      val c = s.charAt(i)
      if (c >= 55296 && c <= 56319) {
        charval = ((c - 55296) * 1024) + (s.charAt(i + 1).toInt - 56320) + 
          65536
        i += 1
      } else {
        charval = c
      }
      array(o += 1) = charval
    }
    array
  }

  /**
   * Contract an array of integers containing Unicode codepoints into a Java string
   *
   * @param codes an array of integers representing the Unicode code points
   * @param used  the number of items in the array that are actually used
   * @return the constructed string
   */
  def contract(codes: Array[Int], used: Int): CharSequence = {
    val sb = new FastStringBuffer(codes.length)
    for (i <- 0 until used) {
      if (codes(i) < 65536) {
        sb.append(codes(i).toChar)
      } else {
        sb.append(UTF16CharacterSet.highSurrogate(codes(i)))
        sb.append(UTF16CharacterSet.lowSurrogate(codes(i)))
      }
    }
    sb
  }

  def isValidLanguageCode(`val`: CharSequence): Boolean = {
    val regex = "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"
    (`val`.toString.matches(regex))
  }

  /**
   * Produce a diagnostic representation of the contents of the string
   *
   * @param s the string
   * @return a string in which non-Ascii-printable characters are replaced by \ uXXXX escapes
   */
  def diagnosticDisplay(s: String): String = {
    val fsb = new FastStringBuffer(s.length)
    for (i <- 0 until len) {
      val c = s.charAt(i)
      if (c >= 0x20 && c <= 0x7e) {
        fsb.append(c)
      } else {
        fsb.append("\\u")
        var shift = 12
        while (shift >= 0) {
          fsb.append("0123456789ABCDEF".charAt((c >> shift) & 0xF))
          shift -= 4
        }
      }
    }
    fsb.toString
  }
}

/**
 * An atomic value of type xs:string. This class is also used for types derived from xs:string.
 * Subclasses of StringValue are used for xs:untypedAtomic and xs:anyURI values.
 */
class StringValue protected () extends AtomicValue {

  protected var value: CharSequence = ""

  protected var noSurrogates: Boolean = false

  /**
   * Constructor. Note that although a StringValue may wrap any kind of CharSequence
   * (usually a String, but it can also be, for example, a StringBuffer), the caller
   * is responsible for ensuring that the value is immutable.
   *
   * @param value the String value. Null is taken as equivalent to "".
   */
  def this(value: CharSequence) {
    this()
    this.value = (if (value == null) "" else value)
  }

  /**
   * Assert that the string is known to contain no surrogate pairs
   */
  def setContainsNoSurrogates() {
    noSurrogates = true
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  def getItemType(): AtomicType = AtomicType.STRING

  /**
   * Get the string value as a String
   */
  def getPrimitiveStringValue(): String = {
    (value = value.toString).asInstanceOf[String]
  }

  /**
   * Convert a value to another primitive data type, with control over how validation is
   * handled.
   *
   *
   * @param requiredType type code of the required atomic type. This must not be a namespace-sensitive type.
   * @return the result of the conversion, if successful. If unsuccessful, the value returned
   *         will be a ValidationErrorValue. The caller must check for this condition. No exception is thrown, instead
   *         the exception will be encapsulated within the ErrorValue.
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.STRING || requiredType == AtomicType.ANY_ATOMIC) {
      return this
    }
    convertStringToBuiltInType(value, requiredType)
  }

  /**
   * Get the length of this string, as defined in XPath. This is not the same as the Java length,
   * as a Unicode surrogate pair counts as a single character
   *
   * @return the length of the string in Unicode code points
   */
  def getStringLength(): Int = {
    if (noSurrogates) {
      value.length
    } else {
      val len = getStringLength(value)
      if (len == value.length) {
        noSurrogates = true
      }
      len
    }
  }

  /**
   * Determine whether the string is a zero-length string. This may
   * be more efficient than testing whether the length is equal to zero
   *
   * @return true if the string is zero length
   */
  def isZeroLength(): Boolean = value.length == 0

  /**
   * Determine whether the string contains surrogate pairs
   *
   * @return true if the string contains any non-BMP characters
   */
  def containsSurrogatePairs(): Boolean = {
    (if (noSurrogates) false else getStringLength != value.length)
  }

  /**
   * Ask whether the string is known to contain no surrogate pairs.
   *
   * @return true if it is known to contain no surrogates, false if the answer is not known
   */
  def isKnownToContainNoSurrogates(): Boolean = noSurrogates

  /**
   * Expand a string containing surrogate pairs into an array of 32-bit characters
   *
   * @return an array of integers representing the Unicode code points
   */
  def expand(): Array[Int] = expand(value)

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
   *
   *
   * @param ordered  true if an ordered comparison is required. In this case the result is null if the
   *                 type is unordered; in other cases the returned value will be a Comparable.
   * @param collator Collation to be used for comparing strings
   * @param implicitTimezone
   * @return an Object whose equals() and hashCode() methods implement the XPath comparison semantics
   *         with respect to this atomic value. If ordered is specified, the result will either be null if
   *         no ordering is defined, or will be a Comparable
   */
  def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int): AnyRef = {
    collator.getCollationKey(value.toString)
  }

  /**
   * Determine if two AtomicValues are equal, according to XPath rules. (This method
   * is not used for string comparisons, which are always under the control of a collation.
   * If we get here, it's because there's a type error in the comparison.)
   *
   * @throws ClassCastException always
   */
  override def equals(other: Any): Boolean = {
    throw new ClassCastException("equals on StringValue is not allowed")
  }

  override def hashCode(): Int = value.hashCode

  /**
   * Test whether this StringValue is equal to another under the rules of the codepoint collation
   *
   * @param other the value to be compared with this value
   * @return true if the strings are equal on a codepoint-by-codepoint basis
   */
  def codepointEquals(other: StringValue): Boolean = {
    value.length == other.value.length && value.toString == other.value.toString
  }

  /**
   * Get the effective boolean value of a string
   *
   * @return true if the string has length greater than zero
   */
  def effectiveBooleanValue(): Boolean = value.length > 0

  override def toString(): String = "\"" + value + '\"'
}

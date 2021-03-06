// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.value

import org.orbeon.darius.xpath.`type`.{AtomicType, ConversionResult, ValidationFailure}
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.value.Base64BinaryValue._

object Base64BinaryValue {

  protected[value] def byteArrayHashCode(value: Array[Byte]): Int = {
    var h = 0
    for (i ← 0 until Math.min(value.length, 64)) {
      h = (h << 1) ^ value(i)
    }
    ((h >> 32) ^ h).toInt
  }

  private object Base64Encoder {

    val map = Array[Char]('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/')
  }

  /**
   * Byte to text encoder using base 64 encoding. To create a base 64
   * encoding of a byte stream call [[translate]] for every
   * sequence of bytes and [[getCharArray]] to mark closure of
   * the byte stream and retrieve the text presentation.
   *
   * @author Based on code from the Mozilla Directory SDK
   */
  private class Base64Encoder {

    import Base64Encoder._

    private val out: FastStringBuffer = new FastStringBuffer(FastStringBuffer.MEDIUM)

    private var buf: Int = 0

    private var buf_bytes: Int = 0

    private val line  = new Array[Char](74)

    private var line_length: Int = 0

    private def encode_token(): Unit = {
      val i = line_length
      line(i) = map(0x3F & (buf >> 18))
      line(i + 1) = map(0x3F & (buf >> 12))
      line(i + 2) = map(0x3F & (buf >> 6))
      line(i + 3) = map(0x3F & buf)
      line_length += 4
      buf = 0
      buf_bytes = 0
    }

    private def encode_partial_token(): Unit = {
      val i = line_length
      line(i) = map(0x3F & (buf >> 18))
      line(i + 1) = map(0x3F & (buf >> 12))
      line(i + 2) = if (buf_bytes == 1) '=' else map(0x3F & (buf >> 6))
      line(i + 3) = if (buf_bytes <= 2) '=' else map(0x3F & buf)
      line_length += 4
      buf = 0
      buf_bytes = 0
    }

    private def flush_line(): Unit = {
      out.append(line, 0, line_length)
      line_length = 0
    }

    /**
     * Given a sequence of input bytes, produces a sequence of output bytes
     * using the base64 encoding.  If there are bytes in `out' already, the
     * new bytes are appended, so the caller should do `out.setLength(0)'
     * first if that's desired.
     * @param in the octet sequence to be encoded in Base64
     */
    def translate(in: Array[Byte]): Unit = {
      val in_length = in.length
      for (i ← 0 until in_length) {
        buf = if (buf_bytes == 0) (buf & 0x00FFFF) | (in(i) << 16) else if (buf_bytes == 1) (buf & 0xFF00FF) | ((in(i) << 8) & 0x00FFFF) else (buf & 0xFFFF00) | (in(i) & 0x0000FF)
        if (buf_bytes == 3) {
          encode_token()
          if (line_length >= 72) {
            flush_line()
          }
        }
        if (i == (in_length - 1)) {
          if ((buf_bytes > 0) && (buf_bytes < 3)) encode_partial_token()
          if (line_length > 0) flush_line()
        }
      }
      for (i ← 0 until line.length) line(i) = 0
    }

    def getCharArray: Array[Char] = {
      var ch: Array[Char] = null
      if (buf_bytes != 0) encode_partial_token()
      flush_line()
      for (i ← 0 until line.length) line(i) = 0
      ch = new Array[Char](out.length)
      if (out.length > 0) out.getChars(0, out.length, ch, 0)
      ch
    }
  }

  object Base64Decoder {

    private val NUL: Byte = 127

    private val EOF: Byte = 126

    private val SP: Byte = 125

    private val map = Array[Byte](NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, SP, SP, NUL, NUL, SP, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, SP, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, 62, NUL, NUL, NUL, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, NUL, NUL, NUL, EOF, NUL, NUL, NUL, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, NUL, NUL, NUL, NUL, NUL, NUL, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL)
  }

  /**
   * Base 64 text to byte decoder. To produce the binary  array from
   * base 64 encoding call [[translate]] for each sequence of
   * characters and [[getByteArray]] to mark closure of the
   * character stream and retrieve the binary contents.
   *
   * @author Based on code from the Mozilla Directory SDK
   */
  private class Base64Decoder {

    import Base64Decoder._

    private var out: Array[Byte] = new Array[Byte](128)

    private var used: Int = 0

    private var token = new Array[Byte](4)

    private var bytes = new Array[Byte](3)

    private var token_length: Int = 0

    private def decode_token(): Unit = {
      val num = (token(0) << 18) | (token(1) << 12) | (token(2) << 6) |
        (token(3))
      bytes(0) = (0xFF & (num >> 16)).toByte
      bytes(1) = (0xFF & (num >> 8)).toByte
      bytes(2) = (0xFF & num).toByte
      ensureCapacity(3)
      System.arraycopy(bytes, 0, out, used, 3)
      used += 3
    }

    private def decode_final_token(): Unit = {
      var b0 = token(0)
      var b1 = token(1)
      var b2 = token(2)
      var b3 = token(3)
      var eq_count = 0
      if (b0 == EOF) {
        b0 = 0
        eq_count += 1
      }
      if (b1 == EOF) {
        b1 = 0
        eq_count += 1
      }
      if (b2 == EOF) {
        b2 = 0
        eq_count += 1
      }
      if (b3 == EOF) {
        b3 = 0
        eq_count += 1
      }
      if (eq_count > 2) {
        throw new IllegalArgumentException("The number of '=' signs at the end of a base64 value must not exceed 2")
      }
      if (eq_count == 2 && (b1 & 0x0F) != 0) {
        throw new IllegalArgumentException("In base64, if the value ends with '==' then the last character must be one of [AQgw]")
      }
      if (eq_count == 1 && (b2 & 0x03) != 0) {
        throw new IllegalArgumentException("In base64, if the value ends with '=' then the last character must be one of [AEIMQUYcgkosw048]")
      }
      val num = (b0 << 18) | (b1 << 12) | (b2 << 6) | (b3)
      ensureCapacity(1)
      out(used) = (num >> 16).toByte
      used += 1
      if (eq_count <= 1) {
        ensureCapacity(1)
        out(used) = ((num >> 8) & 0xFF).toByte
        used += 1
        if (eq_count == 0) {
          ensureCapacity(1)
          out(used) = (num & 0xFF).toByte
          used += 1
        }
      }
    }

    private def ensureCapacity(size: Int): Unit = {
      if (used + size >= out.length) {
        val o2 = new Array[Byte](out.length * 2)
        System.arraycopy(out, 0, o2, 0, used)
        out = o2
      }
    }

    /**
     * Decode the base 64 string into a byte array (which can subsequently be accessed using getByteArray()
     * @param str the base 64 string
     * @throws IllegalArgumentException if the base64 string is incorrectly formatted
     */
    def translate(str: CharSequence): Unit = {
      if (token == null) return
      val length = str.length
      var lengthAtEOF: Int = 0
      var found_eq = 0
      for (i ← 0 until length) {
        val c = str.charAt(i)
        if (c > 127) {
          throw new IllegalArgumentException("non-ASCII character in Base64 value (at offset " + i + 
            ')')
        }
        val t = map(c)
        if (t == NUL) {
          throw new IllegalArgumentException("invalid character '" + c + "' in Base64 value (at offset " + 
            i + 
            ')')
        }
        if (found_eq > 0 && t != EOF && t != SP) {
          throw new IllegalArgumentException("In Base64, an '=' character can appear only at the end")
        }
        if (t == EOF) {
          if (found_eq > 0) {
            found_eq += 1
            if (found_eq > 2) {
              throw new IllegalArgumentException("Base64 value can contain at most two '=' characters")
            }
            token_length = (token_length + 1) % 4
          } else {
            found_eq = 1
            lengthAtEOF = token_length
            eof()
            token_length = (lengthAtEOF + 1) % 4
          }
        } else if (t != SP) {
          token(token_length) = t
          token_length += 1
          if (token_length == 4) {
            if (found_eq == 0) {
              decode_token()
            }
            token_length = 0
          }
        }
      }
      if (token_length != 0) {
        throw new IllegalArgumentException("Base64 input must be a multiple of four characters")
      }
    }

    private def eof(): Unit = {
      if (token != null && token_length != 0) {
        while (token_length < 4) {
          token(token_length) = EOF
          token_length += 1
        }
        decode_final_token()
      }
      token_length = 0
      token = new Array[Byte](4)
      bytes = new Array[Byte](3)
    }

    def getByteArray: Array[Byte] = {
      eof()
      val result = new Array[Byte](used)
      System.arraycopy(out, 0, result, 0, used)
      result
    }
  }
}

/**
 * A value of type xs:base64Binary
 */
class Base64BinaryValue(val binaryValue: Array[Byte]) extends AtomicValue {

  def this(s: CharSequence) =
    this(
      {
        val decoder = new Base64Decoder()
        try {
          decoder.translate(s)
        } catch {
          case e: IllegalArgumentException ⇒
            val err = new XPathException(e.getMessage)
            err.setErrorCode("FORG0001")
            throw err
        }
        decoder.getByteArray
      }
    )

  def getItemType: AtomicType = AtomicType.BASE64_BINARY

  /**
   * Convert to target data type
   *
   * @param requiredType an integer identifying the required atomic type
   * @return an AtomicValue, a value of the required type; or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.BASE64_BINARY) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValue)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(getStringValue)
    } else if (requiredType == AtomicType.HEX_BINARY) {
      new HexBinaryValue(binaryValue)
    } else {
      new ValidationFailure("Cannot convert base64Binary to " + requiredType.getDisplayName, "XPTY0004")
    }
  }

  /**
   * Convert to string
   * @return the canonical representation.
   */
  def getPrimitiveStringValue: String = {
    val encoder = new Base64Encoder()
    encoder.translate(binaryValue)
    new String(encoder.getCharArray)
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
    if (ordered) null else this
  }

  /**
   * Test if the two base64Binary values are equal.
   */
  override def equals(other: Any): Boolean = other match {
    case other: Base64BinaryValue ⇒ binaryValue.sameElements(other.binaryValue)
    case _ ⇒ false
  }

  override def hashCode(): Int = byteArrayHashCode(binaryValue)
}

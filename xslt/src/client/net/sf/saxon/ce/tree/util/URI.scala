// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.util

import java.io.IOException
import java.io.Serializable
import URI._
//remove if not needed
import scala.collection.JavaConversions._

object URI {

  /**
   *****************************************************************
   * MalformedURIExceptions are thrown in the process of building a URI
   * or setting fields on a URI when an operation would result in an
   * invalid URI specification.
   *
   *******************************************************************
   */
  @SerialVersionUID(-6695054834342951930L)
  class URISyntaxException extends IOException {

    /**
     ***************************************************************
     * Constructs a <code>MalformedURIException</code> with the
     * specified detail message.
     *
     * @param p_msg the detail message.
     *****************************************************************
     */
    def this(p_msg: String) {
      super(p_msg)
    }
  }

  private val fgLookupTable = new Array[Byte](128)

  private val RESERVED_CHARACTERS = 0x01

  /**
   URI punctuation mark characters: -_.!~*'() - these, combined with
   alphanumerics, constitute the "unreserved" characters
   */
  private val MARK_CHARACTERS = 0x02

  /**
   scheme can be composed of alphanumerics and these characters: +-.
   */
  private val SCHEME_CHARACTERS = 0x04

  /**
   userinfo can be composed of unreserved, escaped and these
   characters: ;:&=+$,
   */
  private val USERINFO_CHARACTERS = 0x08

  /**
   ASCII letter characters
   */
  private val ASCII_ALPHA_CHARACTERS = 0x10

  /**
   ASCII digit characters
   */
  private val ASCII_DIGIT_CHARACTERS = 0x20

  /**
   ASCII hex characters
   */
  private val ASCII_HEX_CHARACTERS = 0x40

  /**
   Path characters
   */
  private val PATH_CHARACTERS = 0x80

  /**
   Mask for alpha-numeric characters
   */
  private val MASK_ALPHA_NUMERIC = ASCII_ALPHA_CHARACTERS | ASCII_DIGIT_CHARACTERS

  /**
   Mask for unreserved characters
   */
  private val MASK_UNRESERVED_MASK = MASK_ALPHA_NUMERIC | MARK_CHARACTERS

  /**
   Mask for URI allowable characters except for %
   */
  private val MASK_URI_CHARACTER = MASK_UNRESERVED_MASK | RESERVED_CHARACTERS

  /**
   Mask for scheme characters
   */
  private val MASK_SCHEME_CHARACTER = MASK_ALPHA_NUMERIC | SCHEME_CHARACTERS

  /**
   Mask for userinfo characters
   */
  private val MASK_USERINFO_CHARACTER = MASK_UNRESERVED_MASK | USERINFO_CHARACTERS

  /**
   Mask for path characters
   */
  private val MASK_PATH_CHARACTER = MASK_UNRESERVED_MASK | PATH_CHARACTERS

  var i = '0'
  while (i <= '9') {
    fgLookupTable(i) |= ASCII_DIGIT_CHARACTERS | ASCII_HEX_CHARACTERS
    i
  }

  var i = 'A'
  while (i <= 'F') {
    fgLookupTable(i) |= ASCII_ALPHA_CHARACTERS | ASCII_HEX_CHARACTERS
    fgLookupTable(i + 0x00000020) |= ASCII_ALPHA_CHARACTERS | ASCII_HEX_CHARACTERS
    i
  }

  var i = 'G'
  while (i <= 'Z') {
    fgLookupTable(i) |= ASCII_ALPHA_CHARACTERS
    fgLookupTable(i + 0x00000020) |= ASCII_ALPHA_CHARACTERS
    i
  }

  val reserved = ";/?:@&=+$,[]"

  for (i ← 0 until reserved.length) {
    fgLookupTable(reserved.charAt(i)) |= RESERVED_CHARACTERS
  }

  val mark = "-_.!~*'()"

  for (i ← 0 until mark.length) {
    fgLookupTable(mark.charAt(i)) |= MARK_CHARACTERS
  }

  fgLookupTable('+') |= SCHEME_CHARACTERS

  fgLookupTable('-') |= SCHEME_CHARACTERS

  fgLookupTable('.') |= SCHEME_CHARACTERS

  val userinfo = ";:&=+$,"

  for (i ← 0 until userinfo.length) {
    fgLookupTable(userinfo.charAt(i)) |= USERINFO_CHARACTERS
  }

  val path = ";/:@&=+$,"

  for (i ← 0 until path.length) {
    fgLookupTable(path.charAt(i)) |= PATH_CHARACTERS
  }

  private var DEBUG: Boolean = false

  /**
   * Determine whether a scheme conforms to the rules for a scheme name.
   * A scheme is conformant if it starts with an alphanumeric, and
   * contains only alphanumerics, '+','-' and '.'.
   *
   * @return true if the scheme is conformant, false otherwise
   */
  private def isConformantSchemeName(p_scheme: String): Boolean = {
    if (p_scheme == null || p_scheme.trim().length == 0) {
      return false
    }
    if (!isAlpha(p_scheme.charAt(0))) {
      return false
    }
    var testChar: Char = 0
    val schemeLength = p_scheme.length
    for (i ← 1 until schemeLength) {
      testChar = p_scheme.charAt(i)
      if (!isSchemeCharacter(testChar)) {
        return false
      }
    }
    true
  }

  /**
   * Determine whether a string is syntactically capable of representing
   * a valid IPv4 address, IPv6 reference or the domain name of a network host.
   * A valid IPv4 address consists of four decimal digit groups separated by a
   * '.'. Each group must consist of one to three digits. See RFC 2732 Section 3,
   * and RFC 2373 Section 2.2, for the definition of IPv6 references. A hostname
   * consists of domain labels (each of which must begin and end with an alphanumeric
   * but may contain '-') separated & by a '.'. See RFC 2396 Section 3.2.2.
   *
   * @return true if the string is a syntactically valid IPv4 address,
   * IPv6 reference or hostname
   */
  private def isWellFormedAddress(address: String): Boolean = {
    if (address == null) {
      return false
    }
    val addrLength = address.length
    if (addrLength == 0) {
      return false
    }
    if (address.startsWith("[")) {
      return isWellFormedIPv6Reference(address)
    }
    if (address.startsWith(".") || address.startsWith("-") || address.endsWith("-")) {
      return false
    }
    var index = address.lastIndexOf('.')
    if (address.endsWith(".")) {
      index = address.substring(0, index).lastIndexOf('.')
    }
    if (index + 1 < addrLength && isDigit(address.charAt(index + 1))) {
      return isWellFormedIPv4Address(address)
    } else {
      if (addrLength > 255) {
        return false
      }
      var testChar: Char = 0
      var labelCharCount = 0
      for (i ← 0 until addrLength) {
        testChar = address.charAt(i)
        if (testChar == '.') {
          if (!isAlphanum(address.charAt(i - 1))) {
            return false
          }
          if (i + 1 < addrLength && !isAlphanum(address.charAt(i + 1))) {
            return false
          }
          labelCharCount = 0
        } else if (!isAlphanum(testChar) && testChar != '-') {
          return false
        } else if (labelCharCount > 63) {
          return false
        }
      }
    }
    true
  }

  /**
   * <p>Determines whether a string is an IPv4 address as defined by
   * RFC 2373, and under the further constraint that it must be a 32-bit
   * address. Though not expressed in the grammar, in order to satisfy
   * the 32-bit address constraint, each segment of the address cannot
   * be greater than 255 (8 bits of information).</p>
   *
   * <p><code>IPv4address = 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT</code></p>
   *
   * @return true if the string is a syntactically valid IPv4 address
   */
  private def isWellFormedIPv4Address(address: String): Boolean = {
    val addrLength = address.length
    var testChar: Char = 0
    var numDots = 0
    var numDigits = 0
    for (i ← 0 until addrLength) {
      testChar = address.charAt(i)
      if (testChar == '.') {
        if ((i > 0 && !isDigit(address.charAt(i - 1))) || 
          (i + 1 < addrLength && !isDigit(address.charAt(i + 1)))) {
          return false
        }
        numDigits = 0
        if (numDots > 3) {
          return false
        }
      } else if (!isDigit(testChar)) {
        return false
      } else if (numDigits > 3) {
        return false
      } else if (numDigits == 3) {
        val first = address.charAt(i - 2)
        val second = address.charAt(i - 1)
        if (!(first < '2' || 
          (first == '2' && 
          (second < '5' || (second == '5' && testChar <= '5'))))) {
          return false
        }
      }
    }
    numDots == 3
  }

  /**
   * <p>Determines whether a string is an IPv6 reference as defined
   * by RFC 2732, where IPv6address is defined in RFC 2373. The
   * IPv6 address is parsed according to Section 2.2 of RFC 2373,
   * with the additional constraint that the address be composed of
   * 128 bits of information.</p>
   *
   * <p><code>IPv6reference = "[" IPv6address "]"</code></p>
   *
   * <p>Note: The BNF expressed in RFC 2373 Appendix B does not
   * accurately describe section 2.2, and was in fact removed from
   * RFC 3513, the successor of RFC 2373.</p>
   *
   * @return true if the string is a syntactically valid IPv6 reference
   */
  private def isWellFormedIPv6Reference(address: String): Boolean = {
    val addrLength = address.length
    var index = 1
    val end = addrLength - 1
    if (!(addrLength > 2 && address.charAt(0) == '[' && address.charAt(end) == ']')) {
      return false
    }
    val counter = Array.ofDim[Int](1)
    index = scanHexSequence(address, index, end, counter)
    if (index == -1) {
      return false
    } else if (index == end) {
      return counter(0) == 8
    }
    if (index + 1 < end && address.charAt(index) == ':') {
      if (address.charAt(index + 1) == ':') {
        if (counter(0) > 8) {
          return false
        }
        index += 2
        if (index == end) {
          return true
        }
      } else {
        return (counter(0) == 6) && 
          isWellFormedIPv4Address(address.substring(index + 1, end))
      }
    } else {
      return false
    }
    val prevCount = counter(0)
    index = scanHexSequence(address, index, end, counter)
    (index == end) || 
      (index != -1 && 
      isWellFormedIPv4Address(address.substring(if (counter(0) > prevCount) index + 1 else index, end)))
  }

  /**
   * Helper method for isWellFormedIPv6Reference which scans the
   * hex sequences of an IPv6 address. It returns the index of the
   * next character to scan in the address, or -1 if the string
   * cannot match a valid IPv6 address.
   *
   * @param address the string to be scanned
   * @param index the beginning index (inclusive)
   * @param end the ending index (exclusive)
   * @param counter a counter for the number of 16-bit sections read
   * in the address
   *
   * @return the index of the next character to scan, or -1 if the
   * string cannot match a valid IPv6 address
   */
  private def scanHexSequence(address: String, 
      index: Int, 
      end: Int, 
      counter: Array[Int]): Int = {
    var testChar: Char = 0
    var numDigits = 0
    val start = index
    while (index < end) {
      testChar = address.charAt(index)
      if (testChar == ':') {
        if (numDigits > 0 && counter(0) > 8) {
          return -1
        }
        if (numDigits == 0 || 
          ((index + 1 < end) && address.charAt(index + 1) == ':')) {
          return index
        }
        numDigits = 0
      } else if (!isHex(testChar)) {
        if (testChar == '.' && numDigits < 4 && numDigits > 0 && counter(0) <= 6) {
          val back = index - numDigits - 1
          return if (back >= start) back else back + 1
        }
        return -1
      } else if (numDigits > 4) {
        return -1
      }
      index
    }
    if (numDigits > 0 && counter(0) <= 8) end else -1
  }

  /**
   * Determine whether a char is a digit.
   *
   * @return true if the char is betweeen '0' and '9', false otherwise
   */
  private def isDigit(p_char: Char): Boolean = p_char >= '0' && p_char <= '9'

  /**
   * Determine whether a character is a hexadecimal character.
   *
   * @return true if the char is betweeen '0' and '9', 'a' and 'f'
   *         or 'A' and 'F', false otherwise
   */
  private def isHex(p_char: Char): Boolean = {
    p_char <= 'f' &&
      (fgLookupTable(p_char) & ASCII_HEX_CHARACTERS) != 0
  }

  /**
   * Determine whether a char is an alphabetic character: a-z or A-Z
   *
   * @return true if the char is alphabetic, false otherwise
   */
  private def isAlpha(p_char: Char): Boolean = {
    (p_char >= 'a' && p_char <= 'z') || (p_char >= 'A' && p_char <= 'Z')
  }

  /**
   * Determine whether a char is an alphanumeric: 0-9, a-z or A-Z
   *
   * @return true if the char is alphanumeric, false otherwise
   */
  private def isAlphanum(p_char: Char): Boolean = {
    p_char <= 'z' && (fgLookupTable(p_char) & MASK_ALPHA_NUMERIC) != 0
  }

  /**
   * Determine whether a character is a reserved character:
   * ';', '/', '?', ':', '@', '&', '=', '+', '$', ',', '[', or ']'
   *
   * @return true if the string contains any reserved characters
   */
  private def isReservedCharacter(p_char: Char): Boolean = {
    p_char <= ']' && (fgLookupTable(p_char) & RESERVED_CHARACTERS) != 0
  }

  /**
   * Determine whether a char is an unreserved character.
   *
   * @return true if the char is unreserved, false otherwise
   */
  private def isUnreservedCharacter(p_char: Char): Boolean = {
    p_char <= '~' &&
      (fgLookupTable(p_char) & MASK_UNRESERVED_MASK) != 0
  }

  /**
   * Determine whether a char is a URI character (reserved or
   * unreserved, not including '%' for escaped octets).
   *
   * @return true if the char is a URI character, false otherwise
   */
  private def isURICharacter(p_char: Char): Boolean = {
    p_char <= '~' && (fgLookupTable(p_char) & MASK_URI_CHARACTER) != 0
  }

  /**
   * Determine whether a char is a scheme character.
   *
   * @return true if the char is a scheme character, false otherwise
   */
  private def isSchemeCharacter(p_char: Char): Boolean = {
    p_char <= 'z' &&
      (fgLookupTable(p_char) & MASK_SCHEME_CHARACTER) != 0
  }

  /**
   * Determine whether a char is a userinfo character.
   *
   * @return true if the char is a userinfo character, false otherwise
   */
  private def isUserinfoCharacter(p_char: Char): Boolean = {
    p_char <= 'z' &&
      (fgLookupTable(p_char) & MASK_USERINFO_CHARACTER) != 0
  }

  /**
   * Determine whether a char is a path character.
   *
   * @return true if the char is a path character, false otherwise
   */
  private def isPathCharacter(p_char: Char): Boolean = {
    p_char <= '~' && (fgLookupTable(p_char) & MASK_PATH_CHARACTER) != 0
  }

  /**
   * Determine whether a given string contains only URI characters (also
   * called "uric" in RFC 2396). uric consist of all reserved
   * characters, unreserved characters and escaped characters.
   *
   * @return true if the string is comprised of uric, false otherwise
   */
  private def isURIString(p_uric: String): Boolean = {
    if (p_uric == null) {
      return false
    }
    val end = p_uric.length
    var testChar = '\0'
    for (i ← 0 until end) {
      testChar = p_uric.charAt(i)
      if (testChar == '%') {
        if (i + 2 >= end || !isHex(p_uric.charAt(i + 1)) || !isHex(p_uric.charAt(i + 2))) {
          return false
        } else {
          i += 2
          //continue
        }
      }
      if (isURICharacter(testChar)) {
        //continue
      } else {
        return false
      }
    }
    true
  }
}

/**
 ********************************************************************
 * A class to represent a Uniform Resource Identifier (URI). This class
 * is designed to handle the parsing of URIs and provide access to
 * the various components (scheme, host, port, userinfo, path, query
 * string and fragment) that may constitute a URI.
 * <p>
 * Parsing of a URI specification is done according to the URI
 * syntax described in
 * <a href="http://www.ietf.org/rfc/rfc2396.txt?number=2396">RFC 2396</a>,
 * and amended by
 * <a href="http://www.ietf.org/rfc/rfc2732.txt?number=2732">RFC 2732</a>.
 * <p>
 * Every absolute URI consists of a scheme, followed by a colon (':'),
 * followed by a scheme-specific part. For URIs that follow the
 * "generic URI" syntax, the scheme-specific part begins with two
 * slashes ("//") and may be followed by an authority segment (comprised
 * of user information, host, and port), path segment, query segment
 * and fragment. Note that RFC 2396 no longer specifies the use of the
 * parameters segment and excludes the "user:password" syntax as part of
 * the authority segment. If "user:password" appears in a URI, the entire
 * user/password string is stored as userinfo.
 * <p>
 * For URIs that do not follow the "generic URI" syntax (e.g. mailto),
 * the entire scheme-specific part is treated as the "path" portion
 * of the URI.
 * <p>
 * Note that, unlike the java.net.URL class, this class does not provide
 * any built-in network access functionality nor does it provide any
 * scheme-specific functionality (for example, it does not know a
 * default port for a specific scheme). Rather, it only knows the
 * grammar and basic set of operations that can be applied to a URI.
 *
 * @version  $Id: URI.java 447241 2006-09-18 05:12:57Z mrglavas $
 *
 *********************************************************************
 */
class URI extends Serializable {

  /**
   Stores the scheme (usually the protocol) for this URI.
   */
  private var m_scheme: String = null

  /**
   If specified, stores the userinfo for this URI; otherwise null
   */
  private var m_userinfo: String = null

  /**
   If specified, stores the host for this URI; otherwise null
   */
  private var m_host: String = null

  /**
   If specified, stores the port for this URI; otherwise -1
   */
  private var m_port: Int = -1

  /**
   If specified, stores the registry based authority for this URI; otherwise -1
   */
  private var m_regAuthority: String = null

  /**
   If specified, stores the path for this URI; otherwise null
   */
  private var m_path: String = null

  /**
   If specified, stores the query string for this URI; otherwise
   null.
   */
  private var m_queryString: String = null

  /**
   If specified, stores the fragment for this URI; otherwise null
   */
  private var m_fragment: String = null

  /**
   * Construct a new URI from another URI. All fields for this URI are
   * set equal to the fields of the URI passed in.
   *
   * @param p_other the URI to copy (cannot be null)
   */
  def this(p_other: URI) {
    this()
    initialize(p_other)
  }

  /**
   * Construct a new URI from a URI specification string. If the
   * specification follows the "generic URI" syntax, (two slashes
   * following the first colon), the specification will be parsed
   * accordingly - setting the scheme, userinfo, host,port, path, query
   * string and fragment fields as necessary. If the specification does
   * not follow the "generic URI" syntax, the specification is parsed
   * into a scheme and scheme-specific part (stored as the path) only.
   *
   * @param p_uriSpec the URI specification string (cannot be null or
   *                  empty)
   *
   * @exception URISyntaxException if p_uriSpec violates any syntax
   *                                   rules
   */
  def this(p_uriSpec: String) {
    this(null.asInstanceOf[URI], p_uriSpec)
  }

  /**
   * Construct a new URI from a URI specification string. If the
   * specification follows the "generic URI" syntax, (two slashes
   * following the first colon), the specification will be parsed
   * accordingly - setting the scheme, userinfo, host,port, path, query
   * string and fragment fields as necessary. If the specification does
   * not follow the "generic URI" syntax, the specification is parsed
   * into a scheme and scheme-specific part (stored as the path) only.
   * Construct a relative URI if boolean is assigned to "true"
   * and p_uriSpec is not valid absolute URI, instead of throwing an exception.
   *
   * @param p_uriSpec the URI specification string (cannot be null or
   *                  empty)
   * @param allowNonAbsoluteURI true to permit non-absolute URIs,
   *                            false otherwise.
   *
   * @exception URISyntaxException if p_uriSpec violates any syntax
   *                                   rules
   */
  def this(p_uriSpec: String, allowNonAbsoluteURI: Boolean) {
    this(null.asInstanceOf[URI], p_uriSpec, allowNonAbsoluteURI)
  }

  /**
   * Construct a new URI from a base URI and a URI specification string.
   * The URI specification string may be a relative URI.
   *
   * @param p_base the base URI (cannot be null if p_uriSpec is null or
   *               empty)
   * @param p_uriSpec the URI specification string (cannot be null or
   *                  empty if p_base is null)
   *
   * @exception URISyntaxException if p_uriSpec violates any syntax
   *                                  rules
   */
  def this(p_base: URI, p_uriSpec: String) {
    this()
    initialize(p_base, p_uriSpec)
  }

  /**
   * Construct a new URI from a base URI and a URI specification string.
   * The URI specification string may be a relative URI.
   * Construct a relative URI if boolean is assigned to "true"
   * and p_uriSpec is not valid absolute URI and p_base is null
   * instead of throwing an exception.
   *
   * @param p_base the base URI (cannot be null if p_uriSpec is null or
   *               empty)
   * @param p_uriSpec the URI specification string (cannot be null or
   *                  empty if p_base is null)
   * @param allowNonAbsoluteURI true to permit non-absolute URIs,
   *                            false otherwise.
   *
   * @exception URISyntaxException if p_uriSpec violates any syntax
   *                                  rules
   */
  def this(p_base: URI, p_uriSpec: String, allowNonAbsoluteURI: Boolean) {
    this()
    initialize(p_base, p_uriSpec, allowNonAbsoluteURI)
  }

  /**
   * Initialize all fields of this URI from another URI.
   *
   * @param p_other the URI to copy (cannot be null)
   */
  private def initialize(p_other: URI): Unit = {
    m_scheme = p_other.getScheme
    m_userinfo = p_other.getUserinfo
    m_host = p_other.getHost
    m_port = p_other.getPort
    m_regAuthority = p_other.getRegBasedAuthority
    m_path = p_other.getPath
    m_queryString = p_other.getQueryString
    m_fragment = p_other.getFragment
  }

  def resolve(relative: String): URI = new URI(this, relative)

  /**
   * Initializes this URI from a base URI and a URI specification string.
   * See RFC 2396 Section 4 and Appendix B for specifications on parsing
   * the URI and Section 5 for specifications on resolving relative URIs
   * and relative paths.
   *
   * @param p_base the base URI (may be null if p_uriSpec is an absolute
   *               URI)
   * @param p_uriSpec the URI spec string which may be an absolute or
   *                  relative URI (can only be null/empty if p_base
   *                  is not null)
   * @param allowNonAbsoluteURI true to permit non-absolute URIs,
   *                         in case of relative URI, false otherwise.
   *
   * @exception URISyntaxException if p_base is null and p_uriSpec
   *                                  is not an absolute URI or if
   *                                  p_uriSpec violates syntax rules
   */
  private def initialize(p_base: URI, p_uriSpec: String, allowNonAbsoluteURI: Boolean): Unit = {
    val uriSpec = p_uriSpec
    val uriSpecLen = if (uriSpec != null) uriSpec.length else 0
    if (p_base == null && uriSpecLen == 0) {
      if (allowNonAbsoluteURI) {
        m_path = ""
        return
      }
      throw new URISyntaxException("Cannot initialize URI with empty parameters.")
    }
    if (uriSpecLen == 0) {
      initialize(p_base)
      return
    }
    var index = 0
    val colonIdx = uriSpec.indexOf(':')
    if (colonIdx != -1) {
      val searchFrom = colonIdx - 1
      val slashIdx = uriSpec.lastIndexOf('/', searchFrom)
      val queryIdx = uriSpec.lastIndexOf('?', searchFrom)
      val fragmentIdx = uriSpec.lastIndexOf('#', searchFrom)
      if (colonIdx == 0 || slashIdx != -1 || queryIdx != -1 || fragmentIdx != -1) {
        if (colonIdx == 0 || 
          (p_base == null && fragmentIdx != 0 && !allowNonAbsoluteURI)) {
          throw new URISyntaxException("No scheme found in URI.")
        }
      } else {
        initializeScheme(uriSpec)
        index = m_scheme.length + 1
        if (colonIdx == uriSpecLen - 1 || uriSpec.charAt(colonIdx + 1) == '#') {
          throw new URISyntaxException("Scheme specific part cannot be empty.")
        }
      }
    } else if (p_base == null && uriSpec.indexOf('#') != 0 && !allowNonAbsoluteURI) {
      throw new URISyntaxException("No scheme found in URI.")
    }
    if (((index + 1) < uriSpecLen) && 
      (uriSpec.charAt(index) == '/' && uriSpec.charAt(index + 1) == '/')) {
      index += 2
      val startPos = index
      var testChar = '\0'
      while (index < uriSpecLen) {
        testChar = uriSpec.charAt(index)
        if (testChar == '/' || testChar == '?' || testChar == '#') {
          //break
        }
        index += 1
      }
      if (index > startPos) {
        if (!initializeAuthority(uriSpec.substring(startPos, index))) {
          index = startPos - 2
        }
      } else {
        m_host = ""
      }
    }
    initializePath(uriSpec, index)
    if (p_base != null) {
      absolutize(p_base)
    }
  }

  /**
   * Initializes this URI from a base URI and a URI specification string.
   * See RFC 2396 Section 4 and Appendix B for specifications on parsing
   * the URI and Section 5 for specifications on resolving relative URIs
   * and relative paths.
   *
   * @param p_base the base URI (may be null if p_uriSpec is an absolute
   *               URI)
   * @param p_uriSpec the URI spec string which may be an absolute or
   *                  relative URI (can only be null/empty if p_base
   *                  is not null)
   *
   * @exception URISyntaxException if p_base is null and p_uriSpec
   *                                  is not an absolute URI or if
   *                                  p_uriSpec violates syntax rules
   */
  private def initialize(p_base: URI, p_uriSpec: String): Unit = {
    val uriSpec = p_uriSpec
    val uriSpecLen = if (uriSpec != null) uriSpec.length else 0
    if (p_base == null && uriSpecLen == 0) {
      throw new URISyntaxException("Cannot initialize URI with empty parameters.")
    }
    if (uriSpecLen == 0) {
      initialize(p_base)
      return
    }
    var index = 0
    val colonIdx = uriSpec.indexOf(':')
    if (colonIdx != -1) {
      val searchFrom = colonIdx - 1
      val slashIdx = uriSpec.lastIndexOf('/', searchFrom)
      val queryIdx = uriSpec.lastIndexOf('?', searchFrom)
      val fragmentIdx = uriSpec.lastIndexOf('#', searchFrom)
      if (colonIdx == 0 || slashIdx != -1 || queryIdx != -1 || fragmentIdx != -1) {
        if (colonIdx == 0 || (p_base == null && fragmentIdx != 0)) {
          throw new URISyntaxException("No scheme found in URI.")
        }
      } else {
        initializeScheme(uriSpec)
        index = m_scheme.length + 1
        if (colonIdx == uriSpecLen - 1 || uriSpec.charAt(colonIdx + 1) == '#') {
          throw new URISyntaxException("Scheme specific part cannot be empty.")
        }
      }
    } else if (p_base == null && uriSpec.indexOf('#') != 0) {
      throw new URISyntaxException("No scheme found in URI.")
    }
    if (((index + 1) < uriSpecLen) && 
      (uriSpec.charAt(index) == '/' && uriSpec.charAt(index + 1) == '/')) {
      index += 2
      val startPos = index
      var testChar = '\0'
      while (index < uriSpecLen) {
        testChar = uriSpec.charAt(index)
        if (testChar == '/' || testChar == '?' || testChar == '#') {
          //break
        }
        index += 1
      }
      if (index > startPos) {
        if (!initializeAuthority(uriSpec.substring(startPos, index))) {
          index = startPos - 2
        }
      } else {
        m_host = ""
      }
    }
    initializePath(uriSpec, index)
    if (p_base != null) {
      absolutize(p_base)
    }
  }

  /**
   * Absolutize URI with given base URI.
   *
   * @param p_base base URI for absolutization
   */
  private def absolutize(p_base: URI): Unit = {
    if (m_path.length == 0 && m_scheme == null && m_host == null && 
      m_regAuthority == null) {
      m_scheme = p_base.getScheme
      m_userinfo = p_base.getUserinfo
      m_host = p_base.getHost
      m_port = p_base.getPort
      m_regAuthority = p_base.getRegBasedAuthority
      m_path = p_base.getPath
      if (m_queryString == null) {
        m_queryString = p_base.getQueryString
        if (m_fragment == null) {
          m_fragment = p_base.getFragment
        }
      }
      return
    }
    if (m_scheme == null) {
      m_scheme = p_base.getScheme
    } else {
      return
    }
    if (m_host == null && m_regAuthority == null) {
      m_userinfo = p_base.getUserinfo
      m_host = p_base.getHost
      m_port = p_base.getPort
      m_regAuthority = p_base.getRegBasedAuthority
    } else {
      return
    }
    if (m_path.length > 0 && m_path.startsWith("/")) {
      return
    }
    var path = ""
    val basePath = p_base.getPath
    if (basePath != null && basePath.length > 0) {
      val lastSlash = basePath.lastIndexOf('/')
      if (lastSlash != -1) {
        path = basePath.substring(0, lastSlash + 1)
      }
    } else if (m_path.length > 0) {
      path = "/"
    }
    path = path.concat(m_path)
    var index = -1
    while ((index = path.indexOf("/./")) != -1) {
      path = path.substring(0, index + 1).concat(path.substring(index + 3))
    }
    if (path.endsWith("/.")) {
      path = path.substring(0, path.length - 1)
    }
    index = 1
    var segIndex = -1
    var tempString: String = null
    while ((index = path.indexOf("/../", index)) > 0) {
      tempString = path.substring(0, path.indexOf("/../"))
      segIndex = tempString.lastIndexOf('/')
      if (segIndex != -1) {
        if (tempString.substring(segIndex) != "..") {
          path = path.substring(0, segIndex + 1).concat(path.substring(index + 4))
          index = segIndex
        } else {
          index += 4
        }
      } else {
        index += 4
      }
    }
    if (path.endsWith("/..")) {
      tempString = path.substring(0, path.length - 3)
      segIndex = tempString.lastIndexOf('/')
      if (segIndex != -1) {
        path = path.substring(0, segIndex + 1)
      }
    }
    m_path = path
  }

  /**
   * Initialize the scheme for this URI from a URI string spec.
   *
   * @param p_uriSpec the URI specification (cannot be null)
   *
   * @exception URISyntaxException if URI does not have a conformant
   *                                  scheme
   */
  private def initializeScheme(p_uriSpec: String): Unit = {
    val uriSpecLen = p_uriSpec.length
    var index = 0
    var scheme: String = null
    var testChar = '\0'
    while (index < uriSpecLen) {
      testChar = p_uriSpec.charAt(index)
      if (testChar == ':' || testChar == '/' || testChar == '?' || 
        testChar == '#') {
        //break
      }
      index += 1
    }
    scheme = p_uriSpec.substring(0, index)
    if (scheme.length == 0) {
      throw new URISyntaxException("No scheme found in URI.")
    } else {
      setScheme(scheme)
    }
  }

  /**
   * Initialize the authority (either server or registry based)
   * for this URI from a URI string spec.
   *
   * @param p_uriSpec the URI specification (cannot be null)
   *
   * @return true if the given string matched server or registry
   * based authority
   */
  private def initializeAuthority(p_uriSpec: String): Boolean = {
    var index = 0
    var start = 0
    val end = p_uriSpec.length
    var testChar = '\0'
    var userinfo: String = null
    if (p_uriSpec.indexOf('@', start) != -1) {
      while (index < end) {
        testChar = p_uriSpec.charAt(index)
        if (testChar == '@') {
          //break
        }
        index += 1
      }
      userinfo = p_uriSpec.substring(start, index)
      index += 1
    }
    var host: String = null
    start = index
    var hasPort = false
    if (index < end) {
      if (p_uriSpec.charAt(start) == '[') {
        val bracketIndex = p_uriSpec.indexOf(']', start)
        index = if (bracketIndex != -1) bracketIndex else end
        if (index + 1 < end && p_uriSpec.charAt(index + 1) == ':') {
          index
          hasPort = true
        } else {
          index = end
        }
      } else {
        val colonIndex = p_uriSpec.lastIndexOf(':', end)
        index = if (colonIndex > start) colonIndex else end
        hasPort = index != end
      }
    }
    host = p_uriSpec.substring(start, index)
    var port = -1
    if (host.length > 0) {
      if (hasPort) {
        index += 1
        start = index
        while (index < end) {
          index += 1
        }
        val portStr = p_uriSpec.substring(start, index)
        if (portStr.length > 0) {
          try {
            port = Integer.parseInt(portStr)
            if (port == -1) port
          } catch {
            case nfe: NumberFormatException ⇒ port = -2
          }
        }
      }
    }
    if (isValidServerBasedAuthority(host, port, userinfo)) {
      m_host = host
      m_port = port
      m_userinfo = userinfo
      return true
    } else if (isValidRegistryBasedAuthority(p_uriSpec)) {
      m_regAuthority = p_uriSpec
      return true
    }
    false
  }

  /**
   * Determines whether the components host, port, and user info
   * are valid as a server authority.
   *
   * @param host the host component of authority
   * @param port the port number component of authority
   * @param userinfo the user info component of authority
   *
   * @return true if the given host, port, and userinfo compose
   * a valid server authority
   */
  private def isValidServerBasedAuthority(host: String, port: Int, userinfo: String): Boolean = {
    if (!isWellFormedAddress(host)) {
      return false
    }
    if (port < -1 || port > 65535) {
      return false
    }
    if (userinfo != null) {
      var index = 0
      val end = userinfo.length
      var testChar = '\0'
      while (index < end) {
        testChar = userinfo.charAt(index)
        if (testChar == '%') {
          if (index + 2 >= end || !isHex(userinfo.charAt(index + 1)) || 
            !isHex(userinfo.charAt(index + 2))) {
            return false
          }
          index += 2
        } else if (!isUserinfoCharacter(testChar)) {
          return false
        }
        index
      }
    }
    true
  }

  /**
   * Determines whether the given string is a registry based authority.
   *
   * @param authority the authority component of a URI
   *
   * @return true if the given string is a registry based authority
   */
  private def isValidRegistryBasedAuthority(authority: String): Boolean = {
    var index = 0
    val end = authority.length
    var testChar: Char = 0
    while (index < end) {
      testChar = authority.charAt(index)
      if (testChar == '%') {
        if (index + 2 >= end || !isHex(authority.charAt(index + 1)) || 
          !isHex(authority.charAt(index + 2))) {
          return false
        }
        index += 2
      } else if (!isPathCharacter(testChar)) {
        return false
      }
      index
    }
    true
  }

  /**
   * Initialize the path for this URI from a URI string spec.
   *
   * @param p_uriSpec the URI specification (cannot be null)
   * @param p_nStartIndex the index to begin scanning from
   *
   * @exception URISyntaxException if p_uriSpec violates syntax rules
   */
  private def initializePath(p_uriSpec: String, p_nStartIndex: Int): Unit = {
    if (p_uriSpec == null) {
      throw new URISyntaxException("Cannot initialize path from null string!")
    }
    var index = p_nStartIndex
    var start = p_nStartIndex
    val end = p_uriSpec.length
    var testChar = '\0'
    if (start < end) {
      if (getScheme == null || p_uriSpec.charAt(start) == '/') {
        while (index < end) {
          testChar = p_uriSpec.charAt(index)
          if (testChar == '%') {
            if (index + 2 >= end || !isHex(p_uriSpec.charAt(index + 1)) || 
              !isHex(p_uriSpec.charAt(index + 2))) {
              throw new URISyntaxException("Path contains invalid escape sequence!")
            }
            index += 2
          } else if (!isPathCharacter(testChar)) {
            if (testChar == '?' || testChar == '#') {
              //break
            }
            throw new URISyntaxException("Path contains invalid character: " + testChar)
          }
          index
        }
      } else {
        while (index < end) {
          testChar = p_uriSpec.charAt(index)
          if (testChar == '?' || testChar == '#') {
            //break
          }
          if (testChar == '%') {
            if (index + 2 >= end || !isHex(p_uriSpec.charAt(index + 1)) || 
              !isHex(p_uriSpec.charAt(index + 2))) {
              throw new URISyntaxException("Opaque part contains invalid escape sequence!")
            }
            index += 2
          } else if (!isURICharacter(testChar)) {
            throw new URISyntaxException("Opaque part contains invalid character: " + testChar)
          }
          index
        }
      }
    }
    m_path = p_uriSpec.substring(start, index)
    if (testChar == '?') {
      index += 1
      start = index
      while (index < end) {
        testChar = p_uriSpec.charAt(index)
        if (testChar == '#') {
          //break
        }
        if (testChar == '%') {
          if (index + 2 >= end || !isHex(p_uriSpec.charAt(index + 1)) || 
            !isHex(p_uriSpec.charAt(index + 2))) {
            throw new URISyntaxException("Query string contains invalid escape sequence!")
          }
          index += 2
        } else if (!isURICharacter(testChar)) {
          throw new URISyntaxException("Query string contains invalid character: " + testChar)
        }
        index += 1
      }
      m_queryString = p_uriSpec.substring(start, index)
    }
    if (testChar == '#') {
      index += 1
      start = index
      while (index < end) {
        testChar = p_uriSpec.charAt(index)
        if (testChar == '%') {
          if (index + 2 >= end || !isHex(p_uriSpec.charAt(index + 1)) || 
            !isHex(p_uriSpec.charAt(index + 2))) {
            throw new URISyntaxException("Fragment contains invalid escape sequence!")
          }
          index += 2
        } else if (!isURICharacter(testChar)) {
          throw new URISyntaxException("Fragment contains invalid character: " + testChar)
        }
        index += 1
      }
      m_fragment = p_uriSpec.substring(start, index)
    }
  }

  /**
   * Get the scheme for this URI.
   *
   * @return the scheme for this URI
   */
  def getScheme(): String = m_scheme

  /**
   * Get the scheme-specific part for this URI (everything following the
   * scheme and the first colon). See RFC 2396 Section 5.2 for spec.
   *
   * @return the scheme-specific part for this URI
   */
  private def getSchemeSpecificPart(): String = {
    val schemespec = new StringBuffer()
    if (m_host != null || m_regAuthority != null) {
      schemespec.append("//")
      if (m_host != null) {
        if (m_userinfo != null) {
          schemespec.append(m_userinfo)
          schemespec.append('@')
        }
        schemespec.append(m_host)
        if (m_port != -1) {
          schemespec.append(':')
          schemespec.append(m_port)
        }
      } else {
        schemespec.append(m_regAuthority)
      }
    }
    if (m_path != null) {
      schemespec.append(m_path)
    }
    if (m_queryString != null) {
      schemespec.append('?')
      schemespec.append(m_queryString)
    }
    if (m_fragment != null) {
      schemespec.append('#')
      schemespec.append(m_fragment)
    }
    schemespec.toString
  }

  /**
   * Get the userinfo for this URI.
   *
   * @return the userinfo for this URI (null if not specified).
   */
  def getUserinfo(): String = m_userinfo

  /**
   * Get the host for this URI.
   *
   * @return the host for this URI (null if not specified).
   */
  def getHost(): String = m_host

  /**
   * Get the port for this URI.
   *
   * @return the port for this URI (-1 if not specified).
   */
  def getPort(): Int = m_port

  /**
   * Get the registry based authority for this URI.
   *
   * @return the registry based authority (null if not specified).
   */
  def getRegBasedAuthority(): String = m_regAuthority

  /**
   * Get the path for this URI. Note that the value returned is the path
   * only and does not include the query string or fragment.
   *
   * @return the path for this URI.
   */
  def getPath(): String = m_path

  /**
   * Get the query string for this URI.
   *
   * @return the query string for this URI. Null is returned if there
   *         was no "?" in the URI spec, empty string if there was a
   *         "?" but no query string following it.
   */
  def getQueryString(): String = m_queryString

  /**
   * Get the fragment for this URI.
   *
   * @return the fragment for this URI. Null is returned if there
   *         was no "#" in the URI spec, empty string if there was a
   *         "#" but no fragment following it.
   */
  def getFragment(): String = m_fragment

  /**
   * Set the scheme for this URI. The scheme is converted to lowercase
   * before it is set.
   *
   * @param p_scheme the scheme for this URI (cannot be null)
   *
   * @exception URISyntaxException if p_scheme is not a conformant
   *                                  scheme name
   */
  private def setScheme(p_scheme: String): Unit = {
    if (p_scheme == null) {
      throw new URISyntaxException("Cannot set scheme from null string!")
    }
    if (!isConformantSchemeName(p_scheme)) {
      throw new URISyntaxException("The scheme is not conformant.")
    }
    m_scheme = p_scheme.toLowerCase
  }

  /**
   * Determines if the passed-in Object is equivalent to this URI.
   *
   * @param p_test the Object to test for equality.
   *
   * @return true if p_test is a URI with all values equal to this
   *         URI, false otherwise
   */
  override def equals(p_test: Any): Boolean = {
    if (p_test.isInstanceOf[URI]) {
      val testURI = p_test.asInstanceOf[URI]
      if (((m_scheme == null && testURI.m_scheme == null) || 
        (m_scheme != null && testURI.m_scheme != null && m_scheme == testURI.m_scheme)) && 
        ((m_userinfo == null && testURI.m_userinfo == null) || 
        (m_userinfo != null && testURI.m_userinfo != null && m_userinfo == testURI.m_userinfo)) && 
        ((m_host == null && testURI.m_host == null) || 
        (m_host != null && testURI.m_host != null && m_host == testURI.m_host)) && 
        m_port == testURI.m_port && 
        ((m_path == null && testURI.m_path == null) || 
        (m_path != null && testURI.m_path != null && m_path == testURI.m_path)) && 
        ((m_queryString == null && testURI.m_queryString == null) || 
        (m_queryString != null && testURI.m_queryString != null && 
        m_queryString == testURI.m_queryString)) && 
        ((m_fragment == null && testURI.m_fragment == null) || 
        (m_fragment != null && testURI.m_fragment != null && m_fragment == testURI.m_fragment))) {
        return true
      }
    }
    false
  }

  /**
   * Get the URI as a string specification. See RFC 2396 Section 5.2.
   *
   * @return the URI string specification
   */
  override def toString: String = {
    val uriSpecString = new StringBuffer()
    if (m_scheme != null) {
      uriSpecString.append(m_scheme)
      uriSpecString.append(':')
    }
    uriSpecString.append(getSchemeSpecificPart)
    uriSpecString.toString
  }

  /**
   * Returns whether this URI represents an absolute URI.
   *
   * @return true if this URI represents an absolute URI, false
   *         otherwise
   */
  def isAbsolute(): Boolean = m_scheme != null
}

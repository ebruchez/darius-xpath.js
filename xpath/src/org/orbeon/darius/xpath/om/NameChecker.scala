// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.om

import org.orbeon.darius.xpath.expr.z.IntRangeSet
import org.orbeon.darius.xpath.regex.GeneralUnicodeString
import org.orbeon.darius.xpath.trans.{Err, XPathException}

object NameChecker {

  /**
   * Validate a QName, and return the prefix and local name. The local name is checked
   * to ensure it is a valid NCName. The prefix is not checked, on the theory that the caller
   * will look up the prefix to find a URI, and if the prefix is invalid, then no URI will
   * be found.
   *
   * @param qname the lexical QName whose parts are required. Note that leading and trailing
   *              whitespace is not permitted
   * @return an array of two strings, the prefix and the local name. The first
   *         item is a zero-length string if there is no prefix.
   * @throws QNameException if not a valid QName.
   */
  def getQNameParts(qname: CharSequence): Array[String] = {
    val parts = new Array[String](2)
    var colon = -1
    val len = qname.length
    for (i ← 0 until len if qname.charAt(i) == ':') {
      colon = i
      //break
    }
    if (colon < 0) {
      parts(0) = ""
      parts(1) = qname.toString
      if (!isValidNCName(parts(1))) {
        throw new QNameException("Invalid QName " + Err.wrap(qname))
      }
    } else {
      if (colon == 0) {
        throw new QNameException("QName cannot start with colon: " + Err.wrap(qname))
      }
      if (colon == len - 1) {
        throw new QNameException("QName cannot end with colon: " + Err.wrap(qname))
      }
      parts(0) = qname.subSequence(0, colon).toString
      parts(1) = qname.subSequence(colon + 1, len).toString
      if (!isValidNCName(parts(1))) {
        if (!isValidNCName(parts(0))) {
          throw new QNameException("Both the prefix " + Err.wrap(parts(0)) + " and the local part " + 
            Err.wrap(parts(1)) + 
            " are invalid")
        }
        throw new QNameException("Invalid QName local part " + Err.wrap(parts(1)))
      }
    }
    parts
  }

  /**
   * Validate a QName, and return the prefix and local name. Both parts are checked
   * to ensure they are valid NCNames.
   * <p/>
   * <p><i>Used from compiled code</i></p>
   *
   * @param qname the lexical QName whose parts are required. Note that leading and trailing
   *              whitespace is not permitted
   * @return an array of two strings, the prefix and the local name. The first
   *         item is a zero-length string if there is no prefix.
   * @throws XPathException if not a valid QName.
   */
  def checkQNameParts(qname: CharSequence): Array[String] = {
    try {
      val parts = getQNameParts(qname)
      if (parts(0).length > 0 && ! isValidNCName(parts(0))) {
        throw new XPathException("Invalid QName prefix " + Err.wrap(parts(0)))
      }
      parts
    } catch {
      case e: QNameException ⇒
        val err = new XPathException(e.getMessage)
        err.setErrorCode("FORG0001")
        throw err
    }
  }

  private val nameStartRangeStartPoints: Array[Int] = Array('A', '_', 'a', 0xc0, 0xd8, 0xf8, 0x370, 0x37f, 0x200c, 0x2070, 0x2c00, 0x3001, 0xf900, 0xfdf0, 0x10000)

  private val nameStartRangeEndPoints: Array[Int] = Array('Z', '_', 'z', 0xd6, 0xf6, 0x2ff, 0x37d, 0x1fff, 0x200d, 0x218f, 0x2fef, 0xd7ff, 0xfdcf, 0xfffd, 0xeffff)

  private val ncNameStartChars: IntRangeSet = new IntRangeSet(nameStartRangeStartPoints, nameStartRangeEndPoints)

  private val nameRangeStartPoints: Array[Int] = Array('-', '.', '0', 0xb7, 0x300, 0x203f)

  private val nameRangeEndPoints: Array[Int] = Array('-', '.', '9', 0xb7, 0x36f, 0x2040)

  private val ncNameChars: IntRangeSet = new IntRangeSet(nameRangeStartPoints, nameRangeEndPoints)

  def isNCNameStartChar(c: Int): Boolean = ncNameStartChars.contains(c)

  def isNCNameChar(c: Int): Boolean = {
    ncNameStartChars.contains(c) || ncNameChars.contains(c)
  }

  /**
   * Validate whether a given string constitutes a valid NCName, as defined in XML Namespaces.
   *
   * @param ncName the name to be tested. Any whitespace trimming must have already been applied.
   * @return true if the name is a lexically-valid QName
   */
  def isValidNCName(ncName: CharSequence): Boolean = {
    val len = ncName.length
    if (len == 0) {
      return false
    }
    val us = GeneralUnicodeString.makeUnicodeString(ncName)
    if (!isNCNameStartChar(us.charAt(0))) {
      return false
    }
    for (i ← 1 until len if !isNCNameChar(us.charAt(i))) {
      return false
    }
    true
  }

  /**
   * Test whether a character is a valid XML character
   *
   * @param ch the character to be tested
   * @return true if this is a valid character in XML 1.1
   */
  def isValidChar(ch: Int): Boolean = {
    (ch >= 1 && ch <= 0xd7ff) || (ch >= 0xe000 && ch <= 0xfffd) || 
      (ch >= 0x10000 && ch <= 0x10ffff)
  }
}

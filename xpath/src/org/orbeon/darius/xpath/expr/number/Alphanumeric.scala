// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.number

import org.orbeon.darius.xpath.orbeon.Util

object Alphanumeric {

  private var zeroDigits: Array[Int] = Array(0x0030, 0x0660, 0x06f0, 0x0966, 0x09e6, 0x0a66, 0x0ae6, 0x0b66, 0x0be6, 0x0c66, 0x0ce6, 0x0d66, 0x0e50, 0x0ed0, 0x0f20, 0x1040, 0x17e0, 0x1810, 0x1946, 0x19d0, 0xff10, 0x104a0, 0x107ce, 0x107d8, 0x107e2, 0x107ec, 0x107f6)

  /**
   * Determine whether a Unicode codepoint is alphanumeric, that is, whether it is in one of the
   * categories Nd, Nl, No, Lu, Ll, Lt, Lm or Lo
   * @param c the codepoint to be tested
   * @return true if the codepoint is in one of these categories
   */
  def isAlphanumeric(c: Int): Boolean = {
    if (c <= 0x7F) {
      (c >= 0x30 && c <= 0x39) || (c >= 0x41 && c <= 0x5A) || 
        (c >= 0x61 && c <= 0x7A)
    } else if (c <= 0xffff) {
      Util.isLetterOrDigit(c.toChar)
    } else {
      (0 until startAstralAlphaNumeric.length).find(c <= endAstralAlphaNumeric(_)).exists(c >= startAstralAlphaNumeric(_))
    }
  }

  private var startAstralAlphaNumeric: Array[Int] = Array(0x10000, 0x1000D, 0x10028, 0x1003C, 0x1003F, 0x10050, 0x10080, 0x10107, 0x10140, 0x1018A, 0x10300, 0x10320, 0x10330, 0x10380, 0x103A0, 0x103C8, 0x103D1, 0x10400, 0x104A0, 0x10800, 0x10808, 0x1080A, 0x10837, 0x1083C, 0x1083F, 0x10A00, 0x10A10, 0x10A15, 0x10A19, 0x10A40, 0x1D400, 0x1D456, 0x1D49E, 0x1D4A2, 0x1D4A5, 0x1D4A9, 0x1D4AE, 0x1D4BB, 0x1D4BD, 0x1D4C5, 0x1D507, 0x1D50D, 0x1D516, 0x1D51E, 0x1D53B, 0x1D540, 0x1D546, 0x1D54A, 0x1D552, 0x1D6A8, 0x1D6C2, 0x1D6DC, 0x1D6FC, 0x1D716, 0x1D736, 0x1D750, 0x1D770, 0x1D78A, 0x1D7AA, 0x1D7C4, 0x1D7CE, 0x20000, 0x2F800)

  private var endAstralAlphaNumeric: Array[Int] = Array(0x1000B, 0x10026, 0x1003A, 0x1003D, 0x1004D, 0x1005D, 0x100FA, 0x10133, 0x10178, 0x1018A, 0x1031E, 0x10323, 0x1034A, 0x1039D, 0x103C3, 0x103CF, 0x103D5, 0x1049D, 0x104A9, 0x10805, 0x10808, 0x10835, 0x10838, 0x1083C, 0x1083F, 0x10A00, 0x10A13, 0x10A17, 0x10A33, 0x10A47, 0x1D454, 0x1D49C, 0x1D49F, 0x1D4A2, 0x1D4A6, 0x1D4AC, 0x1D4B9, 0x1D4BB, 0x1D4C3, 0x1D505, 0x1D50A, 0x1D514, 0x1D51C, 0x1D539, 0x1D53E, 0x1D544, 0x1D546, 0x1D550, 0x1D6A5, 0x1D6C0, 0x1D6DA, 0x1D6FA, 0x1D714, 0x1D734, 0x1D74E, 0x1D76E, 0x1D788, 0x1D7A8, 0x1D7C2, 0x1D7C9, 0x1D7FF, 0x2A6D6, 0x2FA1D)

  /**
   * Determine whether a character represents a decimal digit and if so, which digit.
   * @param in the Unicode character being tested.
   * @return -1 if it's not a decimal digit, otherwise the digit value.
   */
  def getDigitValue(in: Int): Int = {
    for (z ← 0 until zeroDigits.length if in <= zeroDigits(z) + 9) {
      if (in >= zeroDigits(z)) {
        return in - zeroDigits(z)
      } else {
        return -1
      }
    }
    -1
  }

  /**
   * Determine which digit family a decimal digit belongs to: that is, return the corresponding zero digit.
   * @param in a Unicode character
   * @return if the character is a digit, return the Unicode character that represents zero in the same digit
   * family. Otherwise, return -1.
   */
  def getDigitFamily(in: Int): Int = {
    for (z ← 0 until zeroDigits.length if in <= zeroDigits(z) + 9) {
      if (in >= zeroDigits(z)) {
        return zeroDigits(z)
      } else {
        return -1
      }
    }
    -1
  }
}

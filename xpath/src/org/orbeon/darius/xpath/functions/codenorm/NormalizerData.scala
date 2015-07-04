// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions.codenorm

import java.{util ⇒ ju}

import org.orbeon.darius.xpath.functions.codenorm.NormalizerData._
import org.orbeon.darius.xpath.tree.util.UTF16CharacterSet

object NormalizerData {

  /**
   * Constant for use in getPairwiseComposition
   */
  val NOT_COMPOSITE = '\uFFFF'
}

class NormalizerData(var canonicalClass: ju.Map[Int, Integer],
    var decompose: ju.Map[Int, String],
    var compose: ju.Map[Int, Integer],
    var isCompatibility: BitSet,
    var isExcluded: BitSet) {

  /**
   * Gets the combining class of a character from the
   * Unicode Character Database.
   * @param   ch      the source character
   * @return          value from 0 to 255
   */
  def getCanonicalClass(ch: Int): Int = {
    val i = canonicalClass.get(ch)
    if (i == null) 0 else i.intValue()
  }

  /**
   * Returns the composite of the two characters. If the two
   * characters don't combine, returns NOT_COMPOSITE.
   * Only has to worry about BMP characters, since those are the only ones that can ever compose.
   * @param   first   first character (e.g. 'c')
   * @param   second   second character (e.g. '�' cedilla)
   * @return          composite (e.g. '�')
   */
  def getPairwiseComposition(first: Int, second: Int): Char = {
    if (first < 0 || first > 0x10FFFF || second < 0 || second > 0x10FFFF) return NOT_COMPOSITE
    val i = compose.get((first << 16) | second)
    if (i == null) NormalizerData.NOT_COMPOSITE else i.intValue().toChar
  }

  /**
   * Gets recursive decomposition of a character from the
   * Unicode Character Database.
   * @param   canonical    If true
   *                  bit is on in this byte, then selects the recursive
   *                  canonical decomposition, otherwise selects
   *                  the recursive compatibility and canonical decomposition.
   * @param   ch      the source character
   * @param   buffer  buffer to be filled with the decomposition
   */
  def getRecursiveDecomposition(canonical: Boolean, ch: Int, buffer: StringBuffer): Unit = {
    val decomp = decompose.get(ch).asInstanceOf[String]
    if (decomp != null && !(canonical && isCompatibility.get(ch))) {
      for (i ← 0 until decomp.length) {
        getRecursiveDecomposition(canonical, decomp.charAt(i), buffer)
      }
    } else {
      if (ch < 65536) {
        buffer.append(ch.toChar)
      } else {
        buffer.append(UTF16CharacterSet.highSurrogate(ch))
        buffer.append(UTF16CharacterSet.lowSurrogate(ch))
      }
    }
  }

  /**
   * Just accessible for testing.
   */
  def getExcluded(ch: Char): Boolean = isExcluded.get(ch)

  /**
   * Just accessible for testing.
   */
  def getRawDecompositionMapping(ch: Char): String = decompose.get(ch.toInt)
}

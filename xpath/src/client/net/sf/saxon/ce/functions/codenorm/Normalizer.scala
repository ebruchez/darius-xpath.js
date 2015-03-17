package client.net.sf.saxon.ce.functions.codenorm

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.UTF16CharacterSet
import Normalizer._
//remove if not needed
import scala.collection.JavaConversions._

object Normalizer {

  /**
   * Masks for the form selector
   */
  val COMPATIBILITY_MASK = 1

  val COMPOSITION_MASK = 2

  /**
   * Normalization Form Selector
   */
  val D = 0

  val C = COMPOSITION_MASK

  val KD = COMPATIBILITY_MASK

  val KC = (COMPATIBILITY_MASK + COMPOSITION_MASK).toByte

  val NO_ACTION = 8

  /**
   * Set the 32-bit character at a particular 16-bit offset in a string buffer,
   * replacing the previous character at that position, and taking account of the
   * fact that either, both, or neither of the characters might be a surrogate pair.
   * @param target the StringBuffer in which the data is to be inserted
   * @param offset the position at which the data is to be inserted
   * @param ch32 the character to be inserted, as a 32-bit Unicode codepoint
   */
  private def setCharAt(target: StringBuffer, offset: Int, ch32: Int) {
    if (ch32 < 65536) {
      if (UTF16CharacterSet.isHighSurrogate(target.charAt(offset))) {
        target.setCharAt(offset, ch32.toChar)
        target.deleteCharAt(offset + 1)
      } else {
        target.setCharAt(offset, ch32.toChar)
      }
    } else {
      if (UTF16CharacterSet.isHighSurrogate(target.charAt(offset))) {
        target.setCharAt(offset, UTF16CharacterSet.highSurrogate(ch32))
        target.setCharAt(offset + 1, UTF16CharacterSet.lowSurrogate(ch32))
      } else {
        target.setCharAt(offset, UTF16CharacterSet.highSurrogate(ch32))
        target.insert(offset + 1, UTF16CharacterSet.lowSurrogate(ch32))
      }
    }
  }

  /**
   * Contains normalization data from the Unicode Character Database.
   */
  private var data: NormalizerData = null
}

/**
 * Implements Unicode Normalization Forms C, D, KC, KD.
 */
class Normalizer(var form: Byte, config: Configuration) {

  if (data == null) {
    data = UnicodeDataParserFromXML.build(config)
  }

  /**
   * Normalizes text according to the chosen form,
   * replacing contents of the target buffer.
   * @param   source      the original text, unnormalized
   * @param   target      the resulting normalized text
   * @return the modified target StringBuffer
   */
  private def normalize(source: CharSequence, target: StringBuffer): StringBuffer = {
    if (form == NO_ACTION || source.length == 0) {
      return new StringBuffer(source.toString)
    }
    internalDecompose(source, target)
    if ((form & COMPOSITION_MASK) != 0) {
      internalCompose(target)
    }
    target
  }

  /**
   * Normalizes text according to the chosen form
   * @param   source      the original text, unnormalized
   * @return  target      the resulting normalized text
   */
  def normalize(source: CharSequence): CharSequence = {
    normalize(source, new StringBuffer(source.length + 8))
  }

  /**
   * Decomposes text, either canonical or compatibility,
   * replacing contents of the target buffer.
   //    * @param   form        the normalization form. If COMPATIBILITY_MASK
   //    *                      bit is on in this byte, then selects the recursive
   //    *                      compatibility decomposition, otherwise selects
   //    *                      the recursive canonical decomposition.
   * @param   source      the original text, unnormalized
   * @param   target      the resulting normalized text
   */
  private def internalDecompose(source: CharSequence, target: StringBuffer) {
    val buffer = new StringBuffer(8)
    val canonical = (form & COMPATIBILITY_MASK) == 0
    var ch32: Int = 0
    var i = 0
    while (i < source.length) {
      buffer.setLength(0)
      ch32 = source.charAt(i += 1)
      if (UTF16CharacterSet.isHighSurrogate(ch32)) {
        val low = source.charAt(i += 1)
        ch32 = UTF16CharacterSet.combinePair(ch32.toChar, low)
      }
      data.getRecursiveDecomposition(canonical, ch32, buffer)
      var ch: Int = 0
      var j = 0
      while (j < buffer.length) {
        ch = buffer.charAt(j += 1)
        if (UTF16CharacterSet.isHighSurrogate(ch)) {
          val low = buffer.charAt(j += 1)
          ch = UTF16CharacterSet.combinePair(ch.toChar, low)
        }
        val chClass = data.getCanonicalClass(ch)
        var k = target.length
        if (chClass != 0) {
          var ch2: Int = 0
          while (k > 0) {
            var step = 1
            ch2 = target.charAt(k - 1)
            if (UTF16CharacterSet.isSurrogate(ch2)) {
              step = 2
              val high = target.charAt(k - 2)
              ch2 = UTF16CharacterSet.combinePair(high, ch2.toChar)
            }
            if (data.getCanonicalClass(ch2) <= chClass) //break
            k -= step
          }
        }
        if (ch < 65536) {
          target.insert(k, ch.toChar)
        } else {
          val chars = Array(UTF16CharacterSet.highSurrogate(ch), UTF16CharacterSet.lowSurrogate(ch))
          target.insert(k, chars)
        }
      }
    }
  }

  /**
   * Composes text in place. Target must already
   * have been decomposed.
   * @param   target      input: decomposed text.
   *                      output: the resulting normalized text.
   */
  private def internalCompose(target: StringBuffer) {
    var starterPos = 0
    var starterCh = target.charAt(0)
    var compPos = 1
    if (UTF16CharacterSet.isHighSurrogate(starterCh)) {
      starterCh = UTF16CharacterSet.combinePair(starterCh.toChar, target.charAt(1))
      compPos += 1
    }
    var lastClass = data.getCanonicalClass(starterCh)
    if (lastClass != 0) lastClass = 256
    var oldLen = target.length
    var ch: Int = 0
    var decompPos = compPos
    while (decompPos < target.length) {
      ch = target.charAt(decompPos += 1)
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        ch = UTF16CharacterSet.combinePair(ch.toChar, target.charAt(decompPos += 1))
      }
      val chClass = data.getCanonicalClass(ch)
      val composite = data.getPairwiseComposition(starterCh, ch)
      if (composite != NormalizerData.NOT_COMPOSITE && (lastClass < chClass || lastClass == 0)) {
        setCharAt(target, starterPos, composite)
        starterCh = composite
      } else {
        if (chClass == 0) {
          starterPos = compPos
          starterCh = ch
        }
        lastClass = chClass
        setCharAt(target, compPos, ch)
        if (target.length != oldLen) {
          decompPos += target.length - oldLen
          oldLen = target.length
        }
        compPos += (if (ch < 65536) 1 else 2)
      }
    }
    target.setLength(compPos)
  }

  /**
   * Just accessible for testing.
   * @param ch a character
   * @return true if the character is an excluded character
   */
  def getExcluded(ch: Char): Boolean = data.getExcluded(ch)

  /**
   * Just accessible for testing.
   * @param ch a character
   * @return the raw decomposition mapping of the character
   */
  def getRawDecompositionMapping(ch: Char): String = data.getRawDecompositionMapping(ch)
}

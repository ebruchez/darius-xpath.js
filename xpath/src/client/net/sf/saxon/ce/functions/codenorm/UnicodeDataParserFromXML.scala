// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions.codenorm

import client.net.sf.saxon.ce.om.{Axis, NodeInfo}
import client.net.sf.saxon.ce.orbeon.{ArrayList, Configuration, HashMap, Map}
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.value.Whitespace

import scala.util.control.Breaks

object UnicodeDataParserFromXML {

  /**
   * Called exactly once by NormalizerData to build the static data
   */
  def build(config: Configuration): NormalizerData = {
    val doc = config.buildDocument("normalizationData.xml")
    val isExcluded = new BitSet(128000)
    val isCompatibility = new BitSet(128000)
    var canonicalClassKeys: NodeInfo = null
    var canonicalClassValues: NodeInfo = null
    var decompositionKeys: NodeInfo = null
    var decompositionValues: NodeInfo = null
    val iter = doc.iterateAxis(Axis.DESCENDANT, NodeKindTest.ELEMENT)

    import Breaks._
    breakable {
      while (true) {
        val item = iter.next().asInstanceOf[NodeInfo]
        if (item == null) {
          break()
        }
        if (item.getLocalPart == "CanonicalClassKeys") {
          canonicalClassKeys = item
        } else if (item.getLocalPart == "CanonicalClassValues") {
          canonicalClassValues = item
        } else if (item.getLocalPart == "DecompositionKeys") {
          decompositionKeys = item
        } else if (item.getLocalPart == "DecompositionValues") {
          decompositionValues = item
        } else if (item.getLocalPart == "ExclusionList") {
          readExclusionList(item.getStringValue, isExcluded)
        } else if (item.getLocalPart == "CompatibilityList") {
          readCompatibilityList(item.getStringValue, isCompatibility)
        }
      }
  }
    val canonicalClass = new HashMap[Int, Integer](400)
    readCanonicalClassTable(canonicalClassKeys.getStringValue, canonicalClassValues.getStringValue, canonicalClass)
    val decompose = new HashMap[Int, String](18000)
    val compose = new HashMap[Int, Integer](15000)
    readDecompositionTable(decompositionKeys.getStringValue, decompositionValues.getStringValue, decompose, 
      compose, isExcluded, isCompatibility)
    new NormalizerData(canonicalClass, decompose, compose, isCompatibility, isExcluded)
  }

  /**
   * Reads exclusion list and stores the data
   */
  private def readExclusionList(s: String, isExcluded: BitSet): Unit = {
    for (tok <- Whitespace.tokenize(s)) {
      val value = Integer.parseInt(tok, 32)
      isExcluded.set(value)
    }
  }

  /**
   * Reads compatibility list and stores the data
   */
  private def readCompatibilityList(s: String, isCompatible: BitSet): Unit = {
    for (tok <- Whitespace.tokenize(s)) {
      val value = Integer.parseInt(tok, 32)
      isCompatible.set(value)
    }
  }

  /**
   * Read canonical class table (mapping from character codes to their canonical class)
   */
  private def readCanonicalClassTable(keyString: String, valueString: String, canonicalClasses: Map[Int, Integer]): Unit = {
    val keys = new ArrayList[Int](5000)
    for (tok <- Whitespace.tokenize(keyString)) {
      val value = Integer.parseInt(tok, 32)
      keys.add(value)//ORBEON was Integer.valueOf
    }
    var k = 0
    for (tok <- Whitespace.tokenize(valueString)) {
      var clss: Int = 0
      var repeat = 1
      val star = tok.indexOf('*')
      if (star < 0) {
        clss = Integer.parseInt(tok, 32)
      } else {
        repeat = Integer.parseInt(tok.substring(0, star))
        clss = Integer.parseInt(tok.substring(star + 1), 32)
      }
      for (i <- 0 until repeat) {
        canonicalClasses.put(keys.get(k).intValue(), clss)
        k += 1
      }
    }
  }

  /**
   * Read canonical class table (mapping from character codes to their canonical class)
   */
  private def readDecompositionTable(decompositionKeyString: String, 
      decompositionValuesString: String, 
      decompose: Map[Int, String],
      compose: Map[Int, Integer],
      isExcluded: BitSet, 
      isCompatibility: BitSet): Unit = {
    var k = 0
    val values = new ArrayList[String](1000)
    for (tok <- Whitespace.tokenize(decompositionValuesString)) {
      var value = ""
      var c = 0
      while (c < tok.length) {
        val h0 = tok.charAt(c)
        c += 1
        val h1 = tok.charAt(c)
        c += 1
        val h2 = tok.charAt(c)
        c += 1
        val h3 = tok.charAt(c)
        c += 1
        val code = ("0123456789abcdef".indexOf(h0) << 12) + ("0123456789abcdef".indexOf(h1) << 8) + 
          ("0123456789abcdef".indexOf(h2) << 4) + 
          ("0123456789abcdef".indexOf(h3))
        value += code.toChar
      }
      values.add(value)
    }
    for (tok <- Whitespace.tokenize(decompositionKeyString)) {
      val key = Integer.parseInt(tok, 32)
      val value = values.get(k)
      k += 1
      decompose.put(key, value)
      if (!isCompatibility.get(key) && !isExcluded.get(key)) {
        var first = '\u0000'
        var second = value.charAt(0)
        if (value.length > 1) {
          first = second
          second = value.charAt(1)
        }
        val pair = (first << 16) | second
        compose.put(pair, key)
      }
    }
    for (sIndex <- 0 until SCount) {
      val TIndex = sIndex % TCount
      var first: Char = 0
      var second: Char = 0
      if (TIndex != 0) {
        first = (SBase + sIndex - TIndex).toChar
        second = (TBase + TIndex).toChar
      } else {
        first = (LBase + sIndex / NCount).toChar
        second = (VBase + (sIndex % NCount) / TCount).toChar
      }
      val pair = (first << 16) | second
      val key = sIndex + SBase
      decompose.put(key, String.valueOf(first) + second)
      compose.put(pair, key)
    }
  }

  /**
   * Hangul composition constants
   */
  private val SBase = 0xAC00
  private val LBase = 0x1100
  private val VBase = 0x1161
  private val TBase = 0x11A7
  private val LCount = 19
  private val VCount = 21
  private val TCount = 28
  private val NCount = VCount * TCount
  private val SCount = LCount * NCount
}

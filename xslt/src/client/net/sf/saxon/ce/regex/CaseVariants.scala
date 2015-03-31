// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import java.util.HashMap
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.om.{Axis, DocumentInfo, NodeInfo}
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.pattern.NameTest
import client.net.sf.saxon.ce.tree.util.Navigator

object CaseVariants {

  private var monoVariants: HashMap[Integer, Integer] = null

  private var polyVariants: HashMap[Integer, Array[Int]] = null

  def build(): Unit = {
    monoVariants = new HashMap[Integer, Integer](2500)
    polyVariants = new HashMap[Integer, Array[Int]](100)
    val config = new Configuration()
    var doc: DocumentInfo = null
    doc = config.buildDocument("casevariants.xml")
    val iter = doc.iterateAxis(Axis.DESCENDANT, new NameTest(Type.ELEMENT, "", "c"))
    while (true) {
      val item = iter.next().asInstanceOf[NodeInfo]
      if (item == null) {
        //break
      }
      val code = Navigator.getAttributeValue(item, "", "n")
      val icode = Integer.parseInt(code, 16)
      val variants = Navigator.getAttributeValue(item, "", "v")
      val vhex = variants.split(",")
      val vint = Array.ofDim[Int](vhex.length)
      for (i ← 0 until vhex.length) {
        vint(i) = Integer.parseInt(vhex(i), 16)
      }
      if (vhex.length == 1) {
        monoVariants.put(icode, vint(0))
      } else {
        polyVariants.put(icode, vint)
      }
    }
  }

  /**
   * Get the case variants of a character
   *
   * @param code the character whose case variants are required
   * @return the case variants of the character, excluding the character itself
   */
  def getCaseVariants(code: Int): Array[Int] = {
    if (monoVariants == null) {
      build()
    }
    val mono = monoVariants.get(code)
    if (mono != null) {
      Array(mono)
    } else {
      val result = polyVariants.get(code)
      if (result == null) {
        EMPTY_INT_ARRAY
      } else {
        result
      }
    }
  }

  private val EMPTY_INT_ARRAY = Array.empty[Int]

  var ROMAN_VARIANTS: Array[Int] = Array(0x0130, 0x0131, 0x212A, 0x017F)
}

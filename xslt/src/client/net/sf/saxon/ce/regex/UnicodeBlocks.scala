// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import java.util.{HashMap, Map}
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.expr.z.{IntRangeSet, IntSet}
import client.net.sf.saxon.ce.om.{Axis, DocumentInfo, NodeInfo}
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.pattern.{NameTest, NodeKindTest}
import client.net.sf.saxon.ce.tree.util.{FastStringBuffer, Navigator}

object UnicodeBlocks {

  private var blocks: Map[String, IntSet] = null

  def getBlock(name: String): IntSet = {
    if (blocks == null) {
      readBlocks(new Configuration())
    }
    var cc = blocks.get(name)
    if (cc != null) {
      return cc
    }
    cc = blocks.get(normalizeBlockName(name))
    cc
  }

  private def normalizeBlockName(name: String): String = {
    val fsb = new FastStringBuffer(name.length)
    for (i ← 0 until name.length) {
      val c = name.charAt(i)
      c match {
        case ' ' | '\t' | '\r' | '\n' | '_' ⇒ //break
        case _ ⇒ fsb.append(c)
      }
    }
    fsb.toString
  }

  private def readBlocks(config: Configuration): Unit = {
    synchronized {
      blocks = new HashMap[String, IntSet](250)
      var doc: DocumentInfo = null
      doc = config.buildDocument("unicodeBlocks.xml")
      val iter = doc.iterateAxis(Axis.DESCENDANT, new NameTest(Type.ELEMENT, "", "block"))
      while (true) {
        val item = iter.next().asInstanceOf[NodeInfo]
        if (item == null) {
          //break
        }
        val blockName = normalizeBlockName(Navigator.getAttributeValue(item, "", "name"))
        var range: IntRangeSet = null
        val ranges = item.iterateAxis(Axis.CHILD, NodeKindTest.ELEMENT)
        while (true) {
          val rangeElement = ranges.next().asInstanceOf[NodeInfo]
          if (rangeElement == null) {
            //break
          }
          val from = Integer.parseInt(Navigator.getAttributeValue(rangeElement, "", "from")
            .substring(2), 16)
          val to = Integer.parseInt(Navigator.getAttributeValue(rangeElement, "", "to")
            .substring(2), 16)
          if (range == null) {
            range = new IntRangeSet(Array(from), Array(to))
          } else {
            range.addRange(from, to)
          }
        }
        blocks.put(blockName, range)
      }
    }
  }
}

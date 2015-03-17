package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.expr.z.IntRangeSet
import client.net.sf.saxon.ce.expr.z.IntSet
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.pattern.NameTest
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.Type
import java.util.HashMap
import java.util.Map
//remove if not needed
import scala.collection.JavaConversions._

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
    for (i <- 0 until name.length) {
      val c = name.charAt(i)
      c match {
        case ' ' | '\t' | '\r' | '\n' | '_' => //break
        case _ => fsb.append(c)
      }
    }
    fsb.toString
  }

  private def readBlocks(config: Configuration) {
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

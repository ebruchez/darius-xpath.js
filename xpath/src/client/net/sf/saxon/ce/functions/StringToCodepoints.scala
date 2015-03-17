package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.ArrayIterator
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.value.IntegerValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the function string-to-codepoints()
 */
class StringToCodepoints extends SystemFunction {

  def newInstance(): StringToCodepoints = new StringToCodepoints()

  def iterate(c: XPathContext): SequenceIterator = {
    val item = argument(0).evaluateItem(c)
    if (item == null) {
      return EmptyIterator.getInstance
    }
    val chars = item.asInstanceOf[StringValue].expand()
    val codes = Array.ofDim[IntegerValue](chars.length)
    for (i <- 0 until chars.length) {
      codes(i) = new IntegerValue(chars(i))
    }
    new ArrayIterator(codes)
  }
}

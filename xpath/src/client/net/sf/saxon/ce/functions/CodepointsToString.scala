package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NameChecker
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.tree.util.UTF16CharacterSet
import client.net.sf.saxon.ce.value.NumericValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the function codepoints-to-string
 */
class CodepointsToString extends SystemFunction {

  def newInstance(): CodepointsToString = new CodepointsToString()

  /**
   * Evaluate
   */
  def evaluateItem(c: XPathContext): Item = {
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    val si = argument(0).iterate(c)
    while (true) {
      val nextInt = si.next().asInstanceOf[NumericValue]
      if (nextInt == null) {
        //break
      }
      val next = nextInt.intValue()
      if (next < 0 || next > Integer.MAX_VALUE || !NameChecker.isValidChar(next.toInt)) {
        throw new XPathException("Invalid XML character [x " + Integer.toHexString(next.toInt) + 
          ']', "FOCH0001")
      }
      if (next < 65536) {
        sb.append(next.toChar)
      } else {
        sb.append(UTF16CharacterSet.highSurrogate(next.toInt))
        sb.append(UTF16CharacterSet.lowSurrogate(next.toInt))
      }
    }
    StringValue.makeStringValue(sb.condense())
  }
}

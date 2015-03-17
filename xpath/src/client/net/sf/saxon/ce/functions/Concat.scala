package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

class Concat extends SystemFunction {

  def newInstance(): Concat = new Concat()

  /**
   * Get the required type of the nth argument
   */
  protected def getRequiredType(arg: Int): SequenceType = getDetails().argumentTypes(0)

  /**
   * Evaluate the function in a string context
   */
  def evaluateAsString(c: XPathContext): CharSequence = evaluateItem(c).getStringValue

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    val numArgs = argument.length
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    for (i <- 0 until numArgs) {
      val `val` = argument(i).evaluateItem(c).asInstanceOf[AtomicValue]
      if (`val` != null) {
        sb.append(`val`.getStringValue)
      }
    }
    StringValue.makeStringValue(sb.condense())
  }
}

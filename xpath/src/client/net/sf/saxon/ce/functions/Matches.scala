package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.regex.ARegularExpression
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.BooleanValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class implements the matches() function for regular expression matching
 */
class Matches extends SystemFunction {

  def newInstance(): Matches = new Matches()

  /**
   * Evaluate the matches() function to give a Boolean value.
   * @param c  The dynamic evaluation context
   * @return the result as a BooleanValue, or null to indicate the empty sequence
   * @throws XPathException on an error
   */
  def evaluateItem(c: XPathContext): Item = {
    var sv0 = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    if (sv0 == null) {
      sv0 = StringValue.EMPTY_STRING
    }
    val pat = argument(1).evaluateItem(c).asInstanceOf[AtomicValue]
    if (pat == null) return null
    var flags: CharSequence = null
    if (argument.length == 2) {
      flags = ""
    } else {
      val sv2 = argument(2).evaluateItem(c).asInstanceOf[AtomicValue]
      if (sv2 == null) return null
      flags = sv2.getStringValue
    }
    try {
      val re = new ARegularExpression(pat.getStringValue, flags.toString, "XP20", null)
      BooleanValue.get(re.containsMatch(sv0.getStringValue))
    } catch {
      case err: XPathException => {
        val de = new XPathException(err)
        de.maybeSetErrorCode("FORX0002")
        throw de
      }
    }
  }
}

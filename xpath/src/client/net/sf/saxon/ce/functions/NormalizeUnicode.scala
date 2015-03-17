package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.codenorm.Normalizer
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.StringValue
import client.net.sf.saxon.ce.value.Whitespace
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implement the XPath normalize-unicode() function
 */
class NormalizeUnicode extends SystemFunction {

  def newInstance(): NormalizeUnicode = new NormalizeUnicode()

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    val sv = argument(0).evaluateItem(c).asInstanceOf[StringValue]
    if (sv == null) {
      return StringValue.EMPTY_STRING
    }
    var fb = Normalizer.C
    if (argument.length == 2) {
      val form = Whitespace.trim(argument(1).evaluateAsString(c))
      if (form.equalsIgnoreCase("NFC")) {
        fb = Normalizer.C
      } else if (form.equalsIgnoreCase("NFD")) {
        fb = Normalizer.D
      } else if (form.equalsIgnoreCase("NFKC")) {
        fb = Normalizer.KC
      } else if (form.equalsIgnoreCase("NFKD")) {
        fb = Normalizer.KD
      } else if (form.length == 0) {
        return sv
      } else {
        dynamicError("Normalization form " + form + " is not supported", "FOCH0003")
      }
    }
    var allASCII = true
    val chars = sv.getStringValue
    var i = chars.length - 1
    while (i >= 0) {
      if (chars.charAt(i) > 127) {
        allASCII = false
        //break
      }
      i -= 1
    }
    if (allASCII) {
      return sv
    }
    val norm = new Normalizer(fb, c.getConfiguration)
    val result = norm.normalize(sv.getStringValue)
    StringValue.makeStringValue(result)
  }
}

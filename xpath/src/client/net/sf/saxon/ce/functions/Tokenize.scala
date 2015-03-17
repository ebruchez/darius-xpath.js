// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.regex.ARegularExpression
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.value.AtomicValue

/**
 * This class implements the tokenize() function for regular expression matching. This returns a
 * sequence of strings representing the unmatched substrings: the separators which match the
 * regular expression are not returned.
 */
class Tokenize extends SystemFunction {

  def newInstance(): Tokenize = new Tokenize()

  /**
   * Iterate over the results of the function
   */
  override def iterate(c: XPathContext): SequenceIterator = {
    var sv = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    if (sv == null) {
      return EmptyIterator.getInstance
    }
    val input = sv.getStringValue
    if (input.length == 0) {
      return EmptyIterator.getInstance
    }
    sv = argument(1).evaluateItem(c).asInstanceOf[AtomicValue]
    val pattern = sv.getStringValue
    var flags: CharSequence = null
    if (argument.length == 2) {
      flags = ""
    } else {
      sv = argument(2).evaluateItem(c).asInstanceOf[AtomicValue]
      flags = sv.getStringValue
    }
    try {
      val re = new ARegularExpression(pattern, flags.toString, "XP20", null)
      if (re.matches("")) {
        dynamicError("The regular expression in tokenize() must not be one that matches a zero-length string", 
          "FORX0003")
      }
      re.tokenize(input)
    } catch {
      case err: XPathException =>
        err.setErrorCode("FORX0002")
        err.maybeSetLocation(this.getSourceLocator)
        throw err
    }
  }
}

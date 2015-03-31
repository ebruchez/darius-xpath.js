// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.tree.iter.{ArrayIterator, EmptyIterator}
import client.net.sf.saxon.ce.value.{IntegerValue, StringValue}

/**
 * This class supports the function string-to-codepoints()
 */
class StringToCodepoints extends SystemFunction {

  def newInstance(): StringToCodepoints = new StringToCodepoints()

  override def iterate(c: XPathContext): SequenceIterator = {
    val item = argument(0).evaluateItem(c)
    if (item == null) {
      return EmptyIterator.getInstance
    }
    val chars = item.asInstanceOf[StringValue].expand()
    val codes = new Array[Item](chars.length)
    for (i <- 0 until chars.length) {
      codes(i) = new IntegerValue(chars(i))
    }
    new ArrayIterator(codes)
  }
}

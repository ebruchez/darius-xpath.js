// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.number

import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.StringValue

/**
 * A RegularGroupFormatter is a NumericGroupFormatter that inserts a separator
 * at constant intervals through a number: for example, a comma after every three
 * digits counting from the right.
 */
class RegularGroupFormatter(var groupSize: Int, var groupSeparator: String) extends NumericGroupFormatter {

  override def format(value: FastStringBuffer): String = {
    val valueEx = StringValue.expand(value)
    val groupSeparatorVal = StringValue.expand(groupSeparator)
    val temp = new FastStringBuffer(FastStringBuffer.TINY)
    if (groupSize > 0) {
      var i = valueEx.length - 1
      var j = 0
      while (i >= 0) {
        if (j != 0 && (j % groupSize) == 0) {
          temp.prependWideChar(groupSeparatorVal(0))
        }
        temp.prependWideChar(valueEx(i))
        i -= 1
        j += 1
      }
      return temp.toString
    }
    value.toString
  }

  /**
   * Get the grouping separator to be used. If more than one is used, return the last.
   * If no grouping separators are used, return null
   *
   * @return the grouping separator
   */
  override def getSeparator: String = groupSeparator
}

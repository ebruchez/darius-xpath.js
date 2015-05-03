// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.number

import org.orbeon.darius.xpath.tree.util.FastStringBuffer

/**
 * A NumericGroupFormatter is responsible for insertion of grouping separators
 * into a formatted number (for example, reformatting "1234" as "1,234").
 */
abstract class NumericGroupFormatter {

  /**
   * Reformat a number to add grouping separators
   * @param value a buffer holding the number to be reformatted
   * @return the reformatted number
   */
  def format(value: FastStringBuffer): String

  /**
   * Get the grouping separator to be used. If more than one is used, return the last.
   * If no grouping separators are used, return null
   * @return the grouping separator
   */
  def getSeparator: String
}

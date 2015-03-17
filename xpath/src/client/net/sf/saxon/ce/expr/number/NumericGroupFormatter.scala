package client.net.sf.saxon.ce.expr.number

import client.net.sf.saxon.ce.tree.util.FastStringBuffer
//remove if not needed
import scala.collection.JavaConversions._

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
  def getSeparator(): String
}

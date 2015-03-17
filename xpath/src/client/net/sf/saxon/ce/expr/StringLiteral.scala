package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Subclass of Literal used specifically for string literals, as this is a common case
 */
class StringLiteral(value: StringValue) extends Literal(value) {

  /**
   * Create a StringLiteral that wraps any CharSequence (including, of course, a String)
   * @param value the CharSequence to be wrapped
   */
  def this(value: CharSequence) {
    super(StringValue.makeStringValue(value))
  }

  /**
   * Get the string represented by this StringLiteral
   * @return the underlying string
   */
  def getStringValue(): String = {
    getValue.asInstanceOf[StringValue].getStringValue
  }

  private def copy(): Expression = {
    new StringLiteral(getValue.asInstanceOf[StringValue])
  }
}

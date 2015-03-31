// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ConversionResult
import client.net.sf.saxon.ce.`type`.ValidationFailure
import AnyURIValue._
//remove if not needed
import scala.collection.JavaConversions._

object AnyURIValue {

  val EMPTY_URI = new AnyURIValue("")
}

/**
 * An XPath value of type xs:anyURI.
 * <p/>
 * <p>This is implemented as a subtype of StringValue even though xs:anyURI is not a subtype of
 * xs:string in the XPath type hierarchy. This enables type promotion from URI to String to happen
 * automatically in most cases where it is appropriate.</p>
 * <p/>
 * <p>This implementation of xs:anyURI allows any string to be contained in the value space,
 * reflecting the specification in XSD 1.1.</p>
 */
class AnyURIValue(value: CharSequence) extends StringValue {

  this.value = if (value == null) "" else Whitespace.collapseWhitespace(value).toString

  def getItemType(): AtomicType = AtomicType.ANY_URI

  /**
   * Convert to target data type
   *
   * @param requiredType integer code representing the item type required
   * @return the result of the conversion, or an ErrorValue
   */
  def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.ANY_ATOMIC || requiredType == AtomicType.ANY_URI) {
      this
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(value)
    } else if (requiredType == AtomicType.STRING) {
      new StringValue(value)
    } else {
      new ValidationFailure("Cannot convert anyURI to " + requiredType.getDisplayName, "XPTY0004")
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr.sort.CodepointCollatingComparer._
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.value.{AtomicValue, StringValue}

object CodepointCollatingComparer {

  private val collator: CodepointCollator = CodepointCollator.getInstance

  private val THE_INSTANCE: CodepointCollatingComparer = new CodepointCollatingComparer()

  /**
   * Get the singular instance of this class
   * @return the singleton instance
   */
  def getInstance: CodepointCollatingComparer = THE_INSTANCE
}

/**
 * An AtomicComparer used for comparing strings, untypedAtomic values, and URIs using the Unicode codepoint
 * collation.
 * A CodepointCollatingComparer is used when it is known in advance that both operands will be of these
 * types, and when the collation is the unicode codepoint collation.
 * This enables all conversions and promotions to be bypassed: the string values of both operands
 * are simply extracted and passed to the collator for comparison.
 *
 * <p>The difference between using this class and using the underlying CodepointCollator directly is that
 * the compare() method in this class expects two instances of AtomicValue as its operands, whereas the
 * underlying class expects two instances of java.lang.String. This class makes use of the extra information
 * held in the wrapping StringValue object, specifically, the knowledge of whether the string contains
 * surrogate pairs.</p>
 *
 * @author Michael H. Kay
 *
 */
class CodepointCollatingComparer private () extends AtomicComparer {

  def getCollator: StringCollator = collator

  /**
   * Compare two AtomicValue objects according to the rules for their data type. UntypedAtomic
   * values are compared as if they were strings; if different semantics are wanted, the conversion
   * must be done by the caller.
   *
   * @param a the first object to be compared. This must be either be an instance
   * of AtomicValue, or null to represent an empty sequence. Empty collates before non-empty.
   * @param b the second object to be compared. This must be either be an instance
   * of AtomicValue, or null to represent an empty sequence.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are not comparable
   */
  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      return if (b == null) 0 else -1
    } else if (b == null) {
      return +1
    }
    val as = a.asInstanceOf[StringValue]
    val bs = b.asInstanceOf[StringValue]
    if (as.containsSurrogatePairs() || bs.containsSurrogatePairs()) {
      collator.compareCS(as.getStringValue, bs.getStringValue)
    } else {
      as.getStringValue.compareTo(bs.getStringValue)
    }
  }

  /**
   * Compare two AtomicValue objects for equality. The values must be instances of xs:string or a type
   * derived from xs:string. The method will also handle xs:untypedAtomic and xs:anyURI values.
   *
   * @param a the first object to be compared.
   * @param b the second object to be compared.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if either value is not xs:string or a subtype
   */
  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = {
    val as = a.asInstanceOf[StringValue]
    val bs = b.asInstanceOf[StringValue]
    as.codepointEquals(bs)
  }
}

package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.lib.StringCollator
import CaseInsensitiveCollator._
//remove if not needed
import scala.collection.JavaConversions._

object CaseInsensitiveCollator {

  private var theInstance: CaseInsensitiveCollator = new CaseInsensitiveCollator()

  def getInstance(): CaseInsensitiveCollator = theInstance
}

/**
 * A collating sequence that uses locale-dependent sorting according to the user's current locale
 */
class CaseInsensitiveCollator extends StringCollator {

  /**
   * Compare two string objects.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   * @throws ClassCastException if the objects are of the wrong type for this Comparer
   */
  def compareStrings(a: String, b: String): Int = {
    String.CASE_INSENSITIVE_ORDER.compare(a, b)
  }

  /**
   * Test whether one string is equal to another, according to the rules
   * of the XPath compare() function. The result is true if and only if the
   * compare() method returns zero: but the implementation may be more efficient
   * than calling compare and testing the result for zero
   *
   * @param s1 the first string
   * @param s2 the second string
   * @return true iff s1 equals s2
   */
  def comparesEqual(s1: String, s2: String): Boolean = s1.equalsIgnoreCase(s2)

  /**
   * Get a collation key for two Strings. The essential property of collation keys
   * is that if two values are equal under the collation, then the collation keys are
   * compare correctly under the equals() method.
   */
  def getCollationKey(s: String): AnyRef = s.toLowerCase()
}

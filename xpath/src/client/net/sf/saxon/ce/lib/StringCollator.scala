package client.net.sf.saxon.ce.lib

//remove if not needed
import scala.collection.JavaConversions._

/**
 * This interface represents a "collation" as defined in XPath, that is, a set of rules for comparing strings
 */
trait StringCollator {

  /**
   * Compare two strings
   * @param o1 the first string
   * @param o2 the second string
   * @return 0 if the strings are considered equal, a negative integer if the first string is less than the second,
   * a positive integer if the first string is greater than the second
   */
  def compareStrings(o1: String, o2: String): Int

  /**
   * Compare two strings for equality. This may be more efficient than using compareStrings and
   * testing whether the result is zero, but it must give the same result
   * @param s1 the first string
   * @param s2 the second string
   * @return true if and only if the strings are considered equal,
   */
  def comparesEqual(s1: String, s2: String): Boolean

  /**
   * Get a collation key for a String. The essential property of collation keys
   * is that if (and only if) two strings are equal under the collation, then
   * comparing the collation keys using the equals() method must return true.
   * @param s the string whose collation key is required
   * @return the collation key
   */
  def getCollationKey(s: String): AnyRef
}

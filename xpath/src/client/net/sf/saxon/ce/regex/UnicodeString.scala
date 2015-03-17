package client.net.sf.saxon.ce.regex

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A character string, supporting characters outside the BMP.
 *
 * There are two implementations, one for BMP strings in which all characters are 16-bit values,
 * and one for more general strings in which all characters are 32-bit values.
 */
trait UnicodeString {

  /**
   * Get a substring of this string
   * @param beginIndex the index of the first character to be included (counting
   * codepoints, not 16-bit characters)
   * @param endIndex the index of the first character to be NOT included (counting
   * codepoints, not 16-bit characters)
   * @return a substring
   */
  def substring(beginIndex: Int, endIndex: Int): UnicodeString

  /**
   * Get the first match for a given character
   * @param search the character to look for
   * @param start the first position to look
   * @return the position of the first occurrence of the sought character, or -1 if not found
   */
  def indexOf(search: Int, start: Int): Int

  /**
   * Get the character at a specified position
   * @param pos the index of the required character (counting
   * codepoints, not 16-bit characters)
   * @return a character (Unicode codepoint) at the specified position.
   */
  def charAt(pos: Int): Int

  /**
   * Get the length of the string, in Unicode codepoints
   * @return the number of codepoints in the string
   */
  def length(): Int

  /**
   * Ask whether a given position is at (or beyond) the end of the string
   * @param pos the index of the required character (counting
   * codepoints, not 16-bit characters)
   * @return <tt>true</tt> iff if the specified index is after the end of the character stream
   */
  def isEnd(pos: Int): Boolean
}

package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.value._
import GeneralUnicodeString._
//remove if not needed
import scala.collection.JavaConversions._

object GeneralUnicodeString {

  def containsSurrogatePairs(value: CharSequence): Boolean = {
    for (i <- 0 until value.length) {
      val c = value.charAt(i).toInt
      if (c >= 55296 && c < 56319) {
        return true
      }
    }
    false
  }

  /**
   * Make a UnicodeString for a given CharSequence
   * @param in the input CharSequence
   * @return a UnicodeString using an appropriate implementation class
   */
  def makeUnicodeString(in: CharSequence): UnicodeString = {
    if (containsSurrogatePairs(in)) {
      new GeneralUnicodeString(in)
    } else {
      new BMPString(in)
    }
  }
}

/**
 * A Unicode string which, in general, may contain non-BMP characters (that is, codepoints
 * outside the range 0-65535)
 */
class GeneralUnicodeString(in: CharSequence) extends UnicodeString {

  private var chars: Array[Int] = StringValue.expand(in)

  private var start: Int = 0

  private var end: Int = chars.length

  private def this(chars: Array[Int], start: Int, end: Int) {
    this()
    this.chars = chars
    this.start = start
    this.end = end
  }

  def substring(beginIndex: Int, endIndex: Int): UnicodeString = {
    if (endIndex > chars.length) {
      throw new IndexOutOfBoundsException("endIndex=" + endIndex + "; sequence size=" + chars.length)
    }
    if (beginIndex < 0 || beginIndex > endIndex) {
      throw new IndexOutOfBoundsException("beginIndex=" + beginIndex + "; endIndex=" + endIndex)
    }
    new GeneralUnicodeString(chars, start + beginIndex, start + endIndex)
  }

  def charAt(pos: Int): Int = chars(start + pos)

  def indexOf(search: Int, pos: Int): Int = {
    (pos until length).find(chars(start + _) == search)
      .getOrElse(-1)
  }

  def length(): Int = end - start

  def isEnd(pos: Int): Boolean = (pos >= (end - start))

  override def toString(): String = {
    var c = chars
    if (start != 0) {
      c = Array.ofDim[Int](end - start)
      System.arraycopy(chars, start, c, 0, end - start)
    }
    StringValue.contract(c, end - start).toString
  }
}

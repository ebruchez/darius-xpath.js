package client.net.sf.saxon.ce.regex

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Encapsulates String as CharacterIterator.
 * <p/>
 * Modified by Saxonica (MHK) to encapsulate any CharSequence, provided the CharSequence contains
 * no surrogate pairs.
 *
 * @author <a href="mailto:ales.novak@netbeans.com">Ales Novak</a>
 * @version CVS $Id: StringCharacterIterator.java 518156 2007-03-14 14:31:26Z vgritsenko $
 */
class BMPString(val src: CharSequence) extends UnicodeString {

  def substring(beginIndex: Int, endIndex: Int): UnicodeString = {
    new BMPString(src.subSequence(beginIndex, endIndex))
  }

  def charAt(pos: Int): Int = src.charAt(pos)

  def indexOf(search: Int, pos: Int): Int = {
    if (search > 65535) {
      -1
    } else {
      (pos until src.length).find(src.charAt(_) == search.toChar)
        .getOrElse(-1)
    }
  }

  def length(): Int = src.length

  def isEnd(pos: Int): Boolean = (pos >= src.length)

  override def toString(): String = src.toString

  def getCharSequence(): CharSequence = src
}

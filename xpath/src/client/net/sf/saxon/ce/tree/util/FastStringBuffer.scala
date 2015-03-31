// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.util

import java.util.Arrays

object FastStringBuffer {
  val TINY = 16
  val SMALL = 64
  val MEDIUM = 256
  val LARGE = 1024
}

/**
 * A simple implementation of a class similar to StringBuffer. Unlike
 * StringBuffer it is not synchronized. It also offers the capability
 * to remove unused space. (This class could possibly be replaced by
 * StringBuilder in JDK 1.5, but using our own class gives more control.)
 */
class FastStringBuffer(initialSize: Int) extends CharSequence {

  private var array: Array[Char] = new Array[Char](initialSize)

  private var used: Int = 0

  /**
   * Create a FastStringBuffer with initial content
   * @param cs the initial content. The buffer is created with just enough capacity for
   * this content (it will be expanded if more content is added later).
   */
  def this(cs: CharSequence) {
    this(cs.length)
    append(cs)
  }

  /**
   * Append the contents of a String to the buffer
   * @param s the String to be appended
   */
  def append(s: String): Unit = {
    val len = s.length
    ensureCapacity(len)
    s.getChars(0, len, array, used)
    used += len
  }

  /**
   * Append the contents of a FastStringBuffer to the buffer
   * @param s the FastStringBuffer to be appended
   */
  def append(s: FastStringBuffer): Unit = {
    val len = s.length
    ensureCapacity(len)
    s.getChars(0, len, array, used)
    used += len
  }

  /**
   * Append the contents of a StringBuffer to the buffer
   * @param s the StringBuffer to be appended
   */
  def append(s: StringBuffer): Unit = {
    val len = s.length
    ensureCapacity(len)
    s.getChars(0, len, array, used)
    used += len
  }

  /**
   * Append the contents of a general CharSequence to the buffer
   * @param s the CharSequence to be appended
   */
  def append(s: CharSequence): Unit = {
    val len = s.length
    ensureCapacity(len)
    s match {
      case s1: String ⇒
        s1.getChars(0, len, array, used)
      case buffer: FastStringBuffer ⇒
        buffer.getChars(0, len, array, used)
      case _ ⇒
        s.toString.getChars(0, len, array, used)
    }
    used += len
  }

  /**
   * Append the contents of a character array to the buffer
   * @param srcArray the array whose contents are to be added
   * @param start the offset of the first character in the array to be copied
   * @param length the number of characters to be copied
   */
  def append(srcArray: Array[Char], start: Int, length: Int): Unit = {
    ensureCapacity(length)
    System.arraycopy(srcArray, start, array, used, length)
    used += length
  }

  /**
   * Append the entire contents of a character array to the buffer
   * @param srcArray the array whose contents are to be added
   */
  def append(srcArray: Array[Char]): Unit = {
    val length = srcArray.length
    ensureCapacity(length)
    System.arraycopy(srcArray, 0, array, used, length)
    used += length
  }

  /**
   * Append a character to the buffer
   * @param ch the character to be added
   */
  def append(ch: Char): Unit = {
    ensureCapacity(1)
    array(used) = ch
    used += 1
  }

  /**
   * Append a wide character to the buffer (as a surrogate pair if necessary)
   * @param ch the character, as a 32-bit Unicode codepoint
   * @return this FastStringBuffer (to allow function chaining)
   */
  def appendWideChar(ch: Int): FastStringBuffer = {
    if (ch > 0xffff) {
      append(UTF16CharacterSet.highSurrogate(ch))
      append(UTF16CharacterSet.lowSurrogate(ch))
    } else {
      append(ch.toChar)
    }
    this
  }

  /**
   * Prepend a wide character to the buffer (as a surrogate pair if necessary)
   * @param ch the character, as a 32-bit Unicode codepoint
   */
  def prependWideChar(ch: Int): Unit = {
    if (ch > 0xffff) {
      prepend(UTF16CharacterSet.lowSurrogate(ch))
      prepend(UTF16CharacterSet.highSurrogate(ch))
    } else {
      prepend(ch.toChar)
    }
  }

  /**
   * Returns the length of this character sequence.  The length is the number
   * of 16-bit <code>char</code>s in the sequence.</p>
   *
   * @return the number of <code>char</code>s in this sequence
   */
  def length(): Int = used

  /**
   * Returns the <code>char</code> value at the specified index.  An index ranges from zero
   * to <tt>length() - 1</tt>.  The first <code>char</code> value of the sequence is at
   * index zero, the next at index one, and so on, as for array
   * indexing. </p>
   * <p/>
   * <p>If the <code>char</code> value specified by the index is a
   * <a href="Character.html#unicode">surrogate</a>, the surrogate
   * value is returned.
   *
   * @param index the index of the <code>char</code> value to be returned
   * @return the specified <code>char</code> value
   * @throws IndexOutOfBoundsException if the <tt>index</tt> argument is negative or not less than
   *                                   <tt>length()</tt>
   */
  def charAt(index: Int): Char = {
    if (index >= used) {
      throw new IndexOutOfBoundsException("" + index)
    }
    array(index)
  }

  /**
   * Returns a new <code>CharSequence</code> that is a subsequence of this sequence.
   * The subsequence starts with the <code>char</code> value at the specified index and
   * ends with the <code>char</code> value at index <tt>end - 1</tt>.  The length
   * (in <code>char</code>s) of the
   * returned sequence is <tt>end - start</tt>, so if <tt>start == end</tt>
   * then an empty sequence is returned. </p>
   *
   * @param start the start index, inclusive
   * @param end   the end index, exclusive
   * @return the specified subsequence
   * @throws IndexOutOfBoundsException if <tt>start</tt> or <tt>end</tt> are negative,
   *                                   if <tt>end</tt> is greater than <tt>length()</tt>,
   *                                   or if <tt>start</tt> is greater than <tt>end</tt>
   */
  def subSequence(start: Int, end: Int): CharSequence = new String(array, start, end - start)

  /**
   * Copies characters from this FastStringBuffer into the destination character
   * array.
   * <p>
   * The first character to be copied is at index <code>srcBegin</code>;
   * the last character to be copied is at index <code>srcEnd-1</code>
   * (thus the total number of characters to be copied is
   * <code>srcEnd-srcBegin</code>). The characters are copied into the
   * subarray of <code>dst</code> starting at index <code>dstBegin</code>
   * and ending at index:
   * <p><blockquote><pre>
   *     dstbegin + (srcEnd-srcBegin) - 1
   * </pre></blockquote>
   *
   * @param      srcBegin   index of the first character in the string
   *                        to copy.
   * @param      srcEnd     index after the last character in the string
   *                        to copy.
   * @param      dst        the destination array.
   * @param      dstBegin   the start offset in the destination array.
   * @throws IndexOutOfBoundsException If any of the following
   *            is true:
   *            <ul><li><code>srcBegin</code> is negative.
   *            <li><code>srcBegin</code> is greater than <code>srcEnd</code>
   *            <li><code>srcEnd</code> is greater than the length of this
   *                string
   *            <li><code>dstBegin</code> is negative
   *            <li><code>dstBegin+(srcEnd-srcBegin)</code> is larger than
   *                <code>dst.length</code></ul>
   */
  def getChars(srcBegin: Int, 
      srcEnd: Int, 
      dst: Array[Char], 
      dstBegin: Int): Unit = {
    if (srcBegin < 0) {
      throw new StringIndexOutOfBoundsException(srcBegin)
    }
    if (srcEnd > used) {
      throw new StringIndexOutOfBoundsException(srcEnd)
    }
    if (srcBegin > srcEnd) {
      throw new StringIndexOutOfBoundsException(srcEnd - srcBegin)
    }
    System.arraycopy(array, srcBegin, dst, dstBegin, srcEnd - srcBegin)
  }

  /**
   * Get the index of the first character equal to a given value
   * @param ch the character to search for
   * @return the position of the first occurrence, or -1 if not found
   */
  def indexOf(ch: Char): Int = {
    (0 until used).find(array(_) == ch).getOrElse(-1)
  }

  /**
   * Convert contents of the FastStringBuffer to a string
   */
  override def toString: String = {
    condense()
    new String(array, 0, used)
  }

  /**
   * Compare equality
   */
  override def equals(other: Any): Boolean = other match {
    case other: CharSequence ⇒ toString == other.toString
    case _ ⇒ false
  }

  /**
   * Generate a hash code
   */
  override def hashCode(): Int = {
    var h = 0
    for (i ← 0 until used) {
      h = 31 * h + array(i)
    }
    h
  }

  /**
   * Get a char[] array containing the characters. The caller should not modify the
   * array.
   * @return a char[] array containing the characters
   */
  def getCharArray: Array[Char] = array

  /**
   * Set the character at a particular offset
   * @param index the index of the character to be set
   * @param ch the new character to overwrite the existing character at that location
   * @throws IndexOutOfBoundsException if int<0 or int>=length()
   */
  def setCharAt(index: Int, ch: Char): Unit = {
    if (index < 0 || index > used) {
      throw new IndexOutOfBoundsException("" + index)
    }
    array(index) = ch
  }

  /**
   * Insert a character at a particular offset
   * @param index the index of the character to be set
   * @param ch the new character to insert at that location
   * @throws IndexOutOfBoundsException if int<0 or int>=length()
   */
  def insertCharAt(index: Int, ch: Char): Unit = {
    if (index < 0 || index > used) {
      throw new IndexOutOfBoundsException("" + index)
    }
    ensureCapacity(1)
    var i = used
    while (i > index) {
      array(i) = array(i - 1)
      i -= 1
    }
    used += 1
    array(index) = ch
  }

  /**
   * Insert wide character at a particular offset
   * @param index the index of the character to be set
   * @param ch the character, as a 32-bit Unicode codepoint
   * @throws IndexOutOfBoundsException if int<0 or int>=length()
   */
  def insertWideCharAt(index: Int, ch: Int): Unit = {
    if (index < 0 || index > used) {
      throw new IndexOutOfBoundsException("" + index)
    }
    if (ch > 0xffff) {
      ensureCapacity(2)
      used += 2
      var i = used
      while (i > index) {
        array(i + 1) = array(i - 1)
        i -= 1
      }
      array(index) = UTF16CharacterSet.highSurrogate(ch)
      array(index + 1) = UTF16CharacterSet.lowSurrogate(ch)
    } else {
      ensureCapacity(1)
      used += 1
      var i = used
      while (i > index) {
        array(i) = array(i - 1)
        i -= 1
      }
      array(index) = ch.toChar
    }
  }

  /**
   * Remove a character at a particular offset
   * @param index the index of the character to be set
   * @throws IndexOutOfBoundsException if int<0 or int>=length()
   */
  def removeCharAt(index: Int): Unit = {
    if (index < 0 || index > used) {
      throw new IndexOutOfBoundsException("" + index)
    }
    used -= 1
    System.arraycopy(array, index + 1, array, index, used - index)
  }

  /**
   * Insert a given character at the start of the buffer
   * @param ch the character to insert
   */
  def prepend(ch: Char): Unit = {
    val a2 = new Array[Char](array.length + 1)
    System.arraycopy(array, 0, a2, 1, used)
    a2(0) = ch
    used += 1
    array = a2
  }

  /**
   * Insert a given character N times at the start of the buffer
   * @param ch the character to insert
   * @param repeat the number of occurrences required. Supplying 0 or a negative number is OK,
   * and is treated as a no-op.
   */
  def prependRepeated(ch: Char, repeat: Int): Unit = {
    if (repeat > 0) {
      val a2 = new Array[Char](array.length + repeat)
      System.arraycopy(array, 0, a2, repeat, used)
      Arrays.fill(a2, 0, repeat, ch)
      used += repeat
      array = a2
    }
  }

  /**
   * Set the length. If this exceeds the current length, this method is a no-op.
   * If this is less than the current length, characters beyond the specified point
   * are deleted.
   * @param length the new length
   */
  def setLength(length: Int): Unit = {
    if (length < 0 || length > used) {
      return
    }
    used = length
  }

  /**
   * Expand the character array if necessary to ensure capacity for appended data
   * @param extra the amount of additional capacity needed, in characters
   */
  def ensureCapacity(extra: Int): Unit = {
    if (used + extra > array.length) {
      var newlen = array.length * 2
      if (newlen < used + extra) {
        newlen = used + extra * 2
      }
      val array2 = new Array[Char](newlen)
      System.arraycopy(array, 0, array2, 0, used)
      array = array2
    }
  }

  /**
   * Remove surplus space from the array. This doesn't reduce the array to the minimum
   * possible size; it only reclaims space if it seems worth doing. Specifically, it
   * contracts the array if the amount of wasted space is more than 256 characters, or
   * more than half the allocated size and more than 20 chars.
   * @return the buffer after removing unused space
   */
  def condense(): CharSequence = {
    if (array.length - used > 256 || 
      (array.length > used * 2 && array.length - used > 20)) {
      val array2 = new Array[Char](used)
      System.arraycopy(array, 0, array2, 0, used)
      array = array2
    }
    this
  }
}

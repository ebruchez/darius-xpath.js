// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import java.util.regex.Pattern

import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.ArrayIterator
import org.orbeon.darius.xpath.value.StringValue

import scala.util.matching.Regex.Match

class ARegularExpression(
  pattern      : CharSequence,
  rawFlags     : String,
  hostLanguage : String
) extends RegularExpression {

  // TODO: flag `x`: "If present, whitespace characters (#x9, #xA, #xD and #x20) in the regular expression are removed
  // prior to matching with one exception: whitespace characters within character class expressions (charClassExpr) are
  // not removed. This flag can be used, for example, to break up long regular expressions into readable lines.

  // UNICODE_CASE, CANON_EQ, UNIX_LINES, LITERAL, UNICODE_CHARACTER_CLASS and COMMENTS

  private def parseFlags(f: String): Int = {
    var flags = 0
    if (f ne null) {
      if (f.indexOf('i') != -1)
        flags |= Pattern.CASE_INSENSITIVE
      if (f.indexOf('m') != -1)
        flags |= Pattern.MULTILINE
      if (f.indexOf('s') != -1)
        flags |= Pattern.DOTALL
    }
    flags
  }

  private val compiledPattern = Pattern.compile(pattern.toString, parseFlags(rawFlags))

  // Used by the matches() function and DurationValue
  def containsMatch(input: CharSequence): Boolean = {
    //regex.findFirstMatchIn(input).isDefined
    compiledPattern.matcher(input).find
  }

  // Used by replace(), tokenize(), and DurationValue
  def matches(input: CharSequence): Boolean =
    compiledPattern.matcher(input).matches

  // Used only by the replace() function
  def replace(input: CharSequence, replacement: CharSequence): CharSequence = {

    class MatchIterator(val source: CharSequence, val pattern: Pattern) extends Iterator[Match] {

      private val matcher = pattern.matcher(source)
      private var nextSeen = false

      private val sb = new java.lang.StringBuffer

      def hasNext: Boolean = {
        if (! nextSeen) nextSeen = matcher.find()
        nextSeen
      }

      def next(): Match = {
        if (! hasNext) throw new NoSuchElementException
        nextSeen = false
        new Match(source, matcher, Nil).force
      }

      def replaced = {
        val newsb = new java.lang.StringBuffer(sb)
        matcher.appendTail(newsb)
        newsb.toString
      }

      def replace(rs: String) = matcher.appendReplacement(sb, rs)
    }

    def replaceAllIn(target: CharSequence, replacer: Match ⇒ String) = {
      val it = new MatchIterator(target, compiledPattern)
      it foreach (md ⇒ it replace replacer(md))
      it.replaced
    }

    replaceAllIn(input, ARegularExpression.processMatch(replacement, _))
  }

  // Used only by the tokenize() function
  def tokenize(input: CharSequence): SequenceIterator =
    new ArrayIterator(compiledPattern.split(input, -1) map StringValue.makeStringValue)
}

object ARegularExpression {

  def make(pattern: String): ARegularExpression =
    new ARegularExpression(pattern, "", "XP20")

  private def processMatch(replacement: CharSequence, currentMatch: Match) = {

    val maxCapture = currentMatch.groupCount

    val sb = new StringBuilder

    var i = 0
    while (i < replacement.length) {
      var ch = replacement.charAt(i)

      if (ch == '\\') {
        i += 1
        ch = replacement.charAt(i)
        if (ch == '\\' || ch == '$') {
          sb.append(ch.toChar)
        } else {
          throw new XPathException("Invalid escape in replacement string")
        }
      } else if (ch == '$') {
        i += 1
        ch = replacement.charAt(i)

        if (! (ch >= '0' && ch <= '9')) {
          throw new XPathException("$ in replacement must be followed by a digit")
        }
        var n = ch - '0'

        if (maxCapture <= 9) {
          if (maxCapture >= n) {
            val captured = currentMatch.group(n)
            if (captured ne null) {
              sb.append(captured)
            }
          } else {
            // append a zero-length string (no-op)
          }
        } else {
          import scala.util.control.Breaks._
          breakable {
            while (true) {
              if (i >= replacement.length) {
                break()
              }
              i += 1
              ch = replacement.charAt(i)
              if (ch >= '0' && ch <= '9') {
                val m: Int = n * 10 + (ch - '0')
                if (m > maxCapture) {
                  i -= 1
                  break()
                } else {
                  n = m
                }
              } else {
                i -= 1
                break()
              }
            }
          }

          sb.append(currentMatch.group(n))
        }
      } else {
        sb.append(ch)
      }

      i += 1
    }
    sb.toString
  }
}
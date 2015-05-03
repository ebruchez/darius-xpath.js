// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.tree.iter.{ArrayIterator, EmptyIterator}
import org.orbeon.darius.xpath.value.StringValue

object ARegexIterator {

  trait OnGroup {

    /**
     * Method to be called when the start of a captured group is encountered
     * @param c the dynamic evaluation context
     * @param groupNumber the group number of the captured group
     */
    def onGroupStart(c: XPathContext, groupNumber: Int): Unit

    /**
     * Method to be called when the end of a captured group is encountered
     * @param c the dynamic evaluation context
     * @param groupNumber the group number of the captured group
     */
    def onGroupEnd(c: XPathContext, groupNumber: Int): Unit
  }
}

/**
 * Class ARegexIterator - provides an iterator over matched and unmatched substrings.
 * This implementation uses the modified Jakarta regular expression engine.
 */
class ARegexIterator(var theString: UnicodeString, var regex: UnicodeString, var matcher: REMatcher)
    extends SequenceIterator {

  private var current: UnicodeString = _

  private var _next: UnicodeString = null

  private var prevEnd: Int = 0

//  private val nestingTable: HashMap[Integer, Integer] = null

  /**
   * Get the next item in the sequence
   * @return the next item in the sequence
   */
  def next(): Item = {
    if (_next == null && prevEnd >= 0) {
      if (matcher.`match`(theString, prevEnd)) {
        val start = matcher.getParenStart(0)
        val end = matcher.getParenEnd(0)
        if (prevEnd == start) {
          _next = null
          current = theString.substring(start, end)
          prevEnd = end
        } else {
          current = theString.substring(prevEnd, start)
          _next = theString.substring(start, end)
        }
      } else {
        if (prevEnd < theString.length) {
          current = theString.substring(prevEnd, theString.length)
          _next = null
        } else {
          current = null
          prevEnd = -1
          return null
        }
        prevEnd = -1
      }
    } else {
      if (prevEnd >= 0) {
        current = _next
        _next = null
        prevEnd = matcher.getParenEnd(0)
      } else {
        current = null
        return null
      }
    }
    currentStringValue()
  }

  private def currentStringValue(): StringValue = {
    if (current.isInstanceOf[BMPString]) {
      StringValue.makeStringValue(current.asInstanceOf[BMPString].getCharSequence)
    } else {
      StringValue.makeStringValue(current.toString)
    }
  }

  def close(): Unit = {
  }

  def getAnother: SequenceIterator = {
    new ARegexIterator(theString, regex, new REMatcher(matcher.getProgram))
  }

  /**
   * Determine whether the current item is a matching item or a non-matching item
   * @return true if the current item (the one most recently returned by next()) is
   * an item that matches the regular expression, or false if it is an item that
   * does not match
   */
  def isMatching(): Boolean = _next == null && prevEnd >= 0

  /**
   * Get a substring that matches a parenthesised group within the regular expression
   * @param number    the number of the group to be obtained
   * @return the substring of the current item that matches the n'th parenthesized group
   * within the regular expression
   */
  def getRegexGroup(number: Int): String = {
    if (!isMatching) {
      return null
    }
    if (number >= matcher.getParenCount || number < 0) return ""
    val us = matcher.getParen(number)
    if (us == null) "" else us.toString
  }

  /**
   * Get a sequence containing all the regex groups (except group 0, because we want to use indexing from 1).
   * This is used by the saxon:analyze-string() higher-order extension function.
   */
  def getRegexGroupIterator(): SequenceIterator = {
    val c = matcher.getParenCount - 1
    if (c == 0) {
      EmptyIterator.getInstance
    } else {
      val groups = Array.ofDim[Item](c)
      var i = 1
      while (i <= groups.length) {
        groups(i - 1) = StringValue.makeStringValue(matcher.getParen(i).toString)
        i += 1
      }
      new ArrayIterator(groups)
    }
  }
//ORBEON unused
//
//  /**
//   * Process a matching substring, performing specified actions at the start and end of each captured
//   * subgroup. This method will always be called when operating in "push" mode; it writes its
//   * result to context.getReceiver(). The matching substring text is all written to the receiver,
//   * interspersed with calls to the [[ARegexIterator.OnGroup]] methods onGroupStart() and onGroupEnd().
//   * @param context the dynamic evaluation context
//   * @param action defines the processing to be performed at the start and end of a group
//   */
//  def processMatchingSubstring(context: XPathContext, action: OnGroup) {
//    val out = context.getReceiver
//    val c = matcher.getParenCount - 1
//    if (c == 0) {
//      out.characters(current.toString)
//    } else {
//      val actions = new HashMap[Integer, List[Integer]](c)
//      var i = 1
//      while (i <= c) {
//        val start = matcher.getParenStart(i) - matcher.getParenStart(0)
//        if (start != -1) {
//          val end = matcher.getParenEnd(i) - matcher.getParenStart(0)
//          if (start < end) {
//            var s = actions.get(start)
//            if (s == null) {
//              s = new ArrayList[Integer](4)
//              actions.put(start, s)
//            }
//            s.add(i)
//            var e = actions.get(end)
//            if (e == null) {
//              e = new ArrayList[Integer](4)
//              actions.put(end, e)
//            }
//            e.add(0, -i)
//          } else {
//            if (nestingTable == null) {
//              computeNestingTable()
//            }
//            val parentGroup = nestingTable.get(i)
//            var s = actions.get(start)
//            if (s == null) {
//              s = new ArrayList[Integer](4)
//              actions.put(start, s)
//              s.add(i)
//              s.add(-i)
//            } else {
//              var pos = s.size
//              for (e ← 0 until s.size if s.get(e) == -parentGroup) {
//                pos = e
//                //break
//              }
//              s.add(pos, -i)
//              s.add(pos, i)
//            }
//          }
//        }
//        i += 1
//      }
//      val buff = new FastStringBuffer(current.length)
//      for (i ← 0 until current.length + 1) {
//        val events = actions.get(i)
//        if (events != null) {
//          if (buff.length > 0) {
//            out.characters(buff)
//            buff.setLength(0)
//          }
//          for (group ← events) {
//            if (group > 0) {
//              action.onGroupStart(context, group)
//            } else {
//              action.onGroupEnd(context, -group)
//            }
//          }
//        }
//        if (i < current.length) {
//          buff.appendWideChar(current.charAt(i))
//        }
//      }
//      if (buff.length > 0) {
//        out.characters(buff)
//      }
//    }
//  }
//
//  /**
//   * Compute a table showing for each captured group number (opening paren in the regex),
//   * the number of its parent group. This is done by reparsing the source of the regular
//   * expression. This is needed when the result of a match includes an empty group, to determine
//   * its position relative to other groups finishing at the same character position.
//   */
//  private def computeNestingTable() {
//    nestingTable = new HashMap[Integer, Integer](16)
//    val s = regex
//    val stack = Array.ofDim[Int](s.length)
//    var tos = 0
//    val group = 1
//    var inBrackets = 0
//    stack(tos += 1) = 0
//    for (i ← 0 until s.length) {
//      val ch = s.charAt(i)
//      if (ch == '\'') {
//        i += 1
//      } else if (ch == '[') {
//        inBrackets += 1
//      } else if (ch == ']') {
//        inBrackets -= 1
//      } else if (ch == '(' && s.charAt(i + 1) != '?' && inBrackets == 0) {
//        nestingTable.put(group, stack(tos - 1))
//        stack(tos += 1) = group += 1
//      } else if (ch == ')' && inBrackets == 0) {
//        tos -= 1
//      }
//    }
//  }
}

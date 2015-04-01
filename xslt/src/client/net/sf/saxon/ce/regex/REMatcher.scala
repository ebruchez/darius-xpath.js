// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.expr.z.IntHashSet
import client.net.sf.saxon.ce.expr.z.IntSet
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import java.util.ArrayList
import java.util.Arrays
import java.util.HashMap
import java.util.List
import REMatcher._

import scala.collection.JavaConversions._

object REMatcher {

  val MAX_PAREN = 16
}

/**
 * RE is an efficient, lightweight regular expression evaluator/matcher
 * class. Regular expressions are pattern descriptions which enable
 * sophisticated matching of strings.  In addition to being able to
 * match a string against a pattern, you can also extract parts of the
 * match.  This is especially useful in text parsing! Details on the
 * syntax of regular expression patterns are given below.
 * <p/>
 * <p/>
 * To compile a regular expression (RE), you can simply construct an RE
 * matcher object from the string specification of the pattern, like this:
 * <p/>
 * <pre>
 *  RE r = new RE("a*b");
 * </pre>
 * <p/>
 * <p/>
 * Once you have done this, you can call either of the RE.match methods to
 * perform matching on a String.  For example:
 * <p/>
 * <pre>
 *  boolean matched = r.match("aaaab");
 * </pre>
 * <p/>
 * will cause the boolean matched to be set to true because the
 * pattern "a*b" matches the string "aaaab".
 * <p/>
 * <p/>
 * If you were interested in the <i>number</i> of a's which matched the
 * first part of our example expression, you could change the expression to
 * "(a*)b".  Then when you compiled the expression and matched it against
 * something like "xaaaab", you would get results like this:
 * <p/>
 * <pre>
 *  RE r = new RE("(a*)b");                  // Compile expression
 *  boolean matched = r.match("xaaaab");     // Match against "xaaaab"
 *
 *  String wholeExpr = r.getParen(0);        // wholeExpr will be 'aaaab'
 *  String insideParens = r.getParen(1);     // insideParens will be 'aaaa'
 *
 *  int startWholeExpr = r.getParenStart(0); // startWholeExpr will be index 1
 *  int endWholeExpr = r.getParenEnd(0);     // endWholeExpr will be index 6
 *  int lenWholeExpr = r.getParenLength(0);  // lenWholeExpr will be 5
 *
 *  int startInside = r.getParenStart(1);    // startInside will be index 1
 *  int endInside = r.getParenEnd(1);        // endInside will be index 5
 *  int lenInside = r.getParenLength(1);     // lenInside will be 4
 * </pre>
 * <p/>
 * You can also refer to the contents of a parenthesized expression
 * within a regular expression itself.  This is called a
 * 'backreference'.  The first backreference in a regular expression is
 * denoted by \1, the second by \2 and so on.  So the expression:
 * <p/>
 * <pre>
 *  ([0-9]+)=\1
 * </pre>
 * <p/>
 * will match any string of the form n=n (like 0=0 or 2=2).
 * <p/>
 * <p/>
 * The full regular expression syntax accepted by RE is as defined in the XSD 1.1
 * specification, modified by the XPath 2.0 or 3.0 specifications.
 * <p/>
 * <p/>
 * <b><font face="times roman">Line terminators</font></b>
 * <br>
 * A line terminator is a one- or two-character sequence that marks
 * the end of a line of the input character sequence. The following
 * are recognized as line terminators:
 * <ul>
 * <li>A newline (line feed) character ('\n'),</li>
 * <li>A carriage-return character followed immediately by a newline character ("\r\n"),</li>
 * <li>A standalone carriage-return character ('\r'),</li>
 * <li>A next-line character (''),</li>
 * <li>A line-separator character (' '), or</li>
 * <li>A paragraph-separator character (' ).</li>
 * </ul>
 * <p/>
 * <p/>
 * RE runs programs compiled by the RECompiler class.  But the RE
 * matcher class does not include the actual regular expression compiler
 * for reasons of efficiency.  In fact, if you want to pre-compile one
 * or more regular expressions, the 'recompile' class can be invoked
 * from the command line to produce compiled output like this:
 * <p/>
 * <pre>
 *    // Pre-compiled regular expression "a*b"
 *    char[] re1Instructions =
 *    {
 *        0x007c, 0x0000, 0x001a, 0x007c, 0x0000, 0x000d, 0x0041,
 *        0x0001, 0x0004, 0x0061, 0x007c, 0x0000, 0x0003, 0x0047,
 *        0x0000, 0xfff6, 0x007c, 0x0000, 0x0003, 0x004e, 0x0000,
 *        0x0003, 0x0041, 0x0001, 0x0004, 0x0062, 0x0045, 0x0000,
 *        0x0000,
 *    };
 *
 *
 *    REProgram re1 = new REProgram(re1Instructions);
 * </pre>
 * <p/>
 * You can then construct a regular expression matcher (RE) object from
 * the pre-compiled expression re1 and thus avoid the overhead of
 * compiling the expression at runtime. If you require more dynamic
 * regular expressions, you can construct a single RECompiler object and
 * re-use it to compile each expression. Similarly, you can change the
 * program run by a given matcher object at any time. However, RE and
 * RECompiler are not threadsafe (for efficiency reasons, and because
 * requiring thread safety in this class is deemed to be a rare
 * requirement), so you will need to construct a separate compiler or
 * matcher object for each thread (unless you do thread synchronization
 * yourself). Once expression compiled into the REProgram object, REProgram
 * can be safely shared across multiple threads and RE objects.
 * <p/>
 * <br><p><br>
 * <p/>
 * <font color="red">
 * <i>ISSUES:</i>
 * <p/>
 * <li>Not *all* possibilities are considered for greediness when backreferences
 * are involved (as POSIX suggests should be the case).  The POSIX RE
 * "(ac*)c*d[ac]*\1", when matched against "acdacaa" should yield a match
 * of acdacaa where \1 is "a".  This is not the case in this RE package,
 * and actually Perl doesn't go to this extent either!  Until someone
 * actually complains about this, I'm not sure it's worth "fixing".
 * If it ever is fixed, test #137 in RETest.txt should be updated.</li>
 * </ul>
 * <p/>
 * <p>This library is based on the Apache Jakarta regex library as downloaded
 * on 3 January 2012. Changes have been made to make the grammar and semantics conform to XSD
 * and XPath rules; these changes are listed in source code comments in the
 * RECompiler source code module.</p>
 * </font>
 *
 * @author <a href="mailto:jonl@muppetlabs.com">Jonathan Locke</a>
 * @author <a href="mailto:ts@sch-fer.de">Tobias Sch&auml;fer</a>
 * @author <a href="mailto:mike@saxonica.com">Michael Kay</a>
 * @see RECompiler
 */
class REMatcher(program: REProgram) {

  var program: REProgram = _

  var search: UnicodeString = _

  var matchFlags: Int = _

  var maxParen: Int = MAX_PAREN

  var parenCount: Int = _

  var startn: Array[Int] = _

  var endn: Array[Int] = _

  var startBackref: Array[Int] = _

  var endBackref: Array[Int] = _

  var history: HashMap[Integer, IntSet] = _

  var instructions: Array[Operation] = _

  var anchoredMatch: Boolean = _

  setProgram(program)

  /**
   * Sets the current regular expression program used by this matcher object.
   *
   * @param program Regular expression program compiled by RECompiler.
   * @see RECompiler
   * @see REProgram
   */
  def setProgram(program: REProgram): Unit = {
    this.program = program
    if (program != null && program.maxParens != -1) {
      this.instructions = program.instructions
      this.maxParen = program.maxParens
    } else {
      this.maxParen = MAX_PAREN
    }
  }

  /**
   * Returns the current regular expression program in use by this matcher object.
   *
   * @return Regular expression program
   * @see #setProgram
   */
  def getProgram(): REProgram = program

  /**
   * Returns the number of parenthesized subexpressions available after a successful match.
   *
   * @return Number of available parenthesized subexpressions
   */
  def getParenCount(): Int = parenCount

  /**
   * Gets the contents of a parenthesized subexpression after a successful match.
   *
   * @param which Nesting level of subexpression
   * @return String
   */
  def getParen(which: Int): UnicodeString = {
    var start: Int = 0
    if (which < parenCount && (start = getParenStart(which)) >= 0) {
      return search.substring(start, getParenEnd(which))
    }
    null
  }

  /**
   * Returns the start index of a given paren level.
   *
   * @param which Nesting level of subexpression
   * @return String index
   */
  def getParenStart(which: Int): Int = {
    if (which < startn.length) {
      return startn(which)
    }
    -1
  }

  /**
   * Returns the end index of a given paren level.
   *
   * @param which Nesting level of subexpression
   * @return String index
   */
  def getParenEnd(which: Int): Int = {
    if (which < endn.length) {
      return endn(which)
    }
    -1
  }

  /**
   * Returns the length of a given paren level.
   *
   * @param which Nesting level of subexpression
   * @return Number of characters in the parenthesized subexpression
   */
  def getParenLength(which: Int): Int = {
    if (which < startn.length) {
      return getParenEnd(which) - getParenStart(which)
    }
    -1
  }

  /**
   * Sets the start of a paren level
   *
   * @param which Which paren level
   * @param i     Index in input array
   */
  protected def setParenStart(which: Int, i: Int): Unit = {
    while (which > startn.length - 1) {
      val s2 = Array.ofDim[Int](startn.length * 2)
      System.arraycopy(startn, 0, s2, 0, startn.length)
      Arrays.fill(s2, startn.length, s2.length, -1)
      startn = s2
    }
    startn(which) = i
  }

  /**
   * Sets the end of a paren level
   *
   * @param which Which paren level
   * @param i     Index in input array
   */
  protected def setParenEnd(which: Int, i: Int): Unit = {
    while (which > endn.length - 1) {
      val e2 = Array.ofDim[Int](endn.length * 2)
      System.arraycopy(endn, 0, e2, 0, endn.length)
      Arrays.fill(e2, endn.length, e2.length, -1)
      endn = e2
    }
    endn(which) = i
  }

  /**
   * Throws an Error representing an internal error condition probably resulting
   * from a bug in the regular expression compiler (or possibly data corruption).
   * In practice, this should be very rare.
   *
   * @param s Error description
   */
  protected def internalError(s: String): Unit = {
    throw new Error("RE internal error: " + s)
  }

  /**
   * Try to match a string against a subset of nodes in the program
   *
   * @param firstNode Node to start at in program
   * @param lastNode  Last valid node (used for matching a subexpression without
   *                  matching the rest of the program as well).
   * @param idx       Starting position in character array
   * @return Final input array index if match succeeded.  -1 if not.
   */
  def matchNodes(firstNode: Int, lastNode: Int, idx: Int): Int = matchNodes(firstNode, idx)

  /**
   * Try to match a string against a subset of nodes in the program. This version
   * has no lastNode argument (which hopefully saves a bit of stack space in the deep recursion)
   *
   * @param node Node to start at in program
   * @param idx  Starting position in character array
   * @return Final input array index if match succeeded.  -1 if not.
   */
  def matchNodes(node: Int, idx: Int): Int = {
    var idxNew: Int = 0
    while (true) {
      val op = instructions(node)
      idxNew = op.exec(this, node, idx)
      if (idxNew != -1) {
        idx = idxNew
      }
      op.nextAction(idxNew) match {
        case Operation.ACTION_RETURN ⇒ return idxNew
        case Operation.ACTION_ADVANCE_TO_NEXT ⇒
          node = op.next
          //continue

        case Operation.ACTION_ADVANCE_TO_FOLLOWING ⇒
          node += 1
          //continue

        case Operation.ACTION_ADVANCE_TO_NEXT_NEXT ⇒
          node = instructions(op.next).next
          //continue

        case _ ⇒ internalError("Unknown action")
      }
      //break
    }
    internalError("Corrupt program")
    -1
  }

  /**
   * Ask whether a particular node has previously visited a particular position
   * in the input string
   *
   * @param idx  the position in the input string
   * @param node the instruction node
   * @return true if this is not the first visit by this instruction to this node
   */
  def beenHereBefore(idx: Int, node: Int): Boolean = {
    var previousVisitors = history.get(idx)
    if (previousVisitors != null && previousVisitors.contains(node)) {
      true
    } else {
      if (previousVisitors == null) {
        previousVisitors = new IntHashSet(4)
        history.put(idx, previousVisitors)
      }
      previousVisitors.add(node)
      false
    }
  }

  /**
   * Match the current regular expression program against the current
   * input string, starting at index i of the input string.  This method
   * is only meant for internal use.
   *
   * @param i The input string index to start matching at
   * @param anchored true if the regex must match all characters up to the end of the string
   * @return True if the input matched the expression
   */
  protected def matchAt(i: Int, anchored: Boolean): Boolean = {
    startn = Array.ofDim[Int](3)
    startn(0) = startn(1) = startn(2) = -1
    endn = Array.ofDim[Int](3)
    endn(0) = endn(1) = endn(2) = -1
    parenCount = 1
    history = new HashMap[Integer, IntSet](search.length)
    anchoredMatch = anchored
    setParenStart(0, i)
    if ((program.optimizationFlags & REProgram.OPT_HASBACKREFS) != 
      0) {
      startBackref = Array.ofDim[Int](maxParen)
      endBackref = Array.ofDim[Int](maxParen)
    }
    var idx: Int = 0
    if ((idx = matchNodes(0, i)) != -1) {
      setParenEnd(0, idx)
      return true
    }
    parenCount = 0
    false
  }

  /**
   * Tests whether the regex matches a string in its entirety, anchored
   * at both ends
   */
  def anchoredMatch(search: UnicodeString): Boolean = {
    this.search = search
    matchAt(0, anchored = true)
  }

  /**
   * Matches the current regular expression program against a character array,
   * starting at a given index.
   *
   * @param search String to match against
   * @param i      Index to start searching at
   * @return True if string matched
   */
  def `match`(search: UnicodeString, i: Int): Boolean = {
    if (program == null) {
      internalError("No RE program to run!")
    }
    this.search = search
    if ((program.optimizationFlags & REProgram.OPT_HASBOL) == REProgram.OPT_HASBOL) {
      if (!program.flags.isMultiLine) {
        return i == 0 && matchAt(i, anchored = false)
      }
      while (!search.isEnd(i)) {
        if (isNewline(i)) {
          //continue
        }
        if (matchAt(i, anchored = false)) {
          return true
        }
        while (!search.isEnd(i)) {
          if (isNewline(i)) {
            //break
          }
          i += 1
        }
        i += 1
      }
      return false
    }
    if (program.prefix == null) {
      while (!search.isEnd(i - 1)) {
        if (matchAt(i, anchored = false)) {
          return true
        }
        i += 1
      }
      false
    } else {
      val prefix = program.prefix
      while (!search.isEnd(i + prefix.length - 1)) {
        var j = i
        var k = 0
        if (program.flags.isCaseIndependent) {
          do {
          } while (equalCaseBlind(search.charAt(j += 1), prefix.charAt(k += 1)) && 
            k < prefix.length);
        } else {
          do {
          } while ((search.charAt(j += 1) == prefix.charAt(k += 1)) && k < prefix.length);
        }
        if (k == prefix.length) {
          if (matchAt(i, anchored = false)) {
            return true
          }
        }
        i += 1
      }
      false
    }
  }

  /**
   * Matches the current regular expression program against a String.
   *
   * @param search String to match against
   * @return True if string matched
   */
  def `match`(search: String): Boolean = {
    `match`(GeneralUnicodeString.makeUnicodeString(search), 0)
  }

  /**
   * Splits a string into an array of strings on regular expression boundaries.
   * This function works the same way as the Perl function of the same name.
   * Given a regular expression of "[ab]+" and a string to split of
   * "xyzzyababbayyzabbbab123", the result would be the array of Strings
   * "[xyzzy, yyz, 123]".
   * <p/>
   * <p>Please note that the first string in the resulting array may be an empty
   * string. This happens when the very first character of input string is
   * matched by the pattern.
   *
   * @param s String to split on this regular exression
   * @return Array of strings
   */
  def split(s: UnicodeString): List[UnicodeString] = {
    val v = new ArrayList[UnicodeString]()
    var pos = 0
    val len = s.length
    while (pos < len && `match`(s, pos)) {
      val start = getParenStart(0)
      var newpos = getParenEnd(0)
      if (newpos == pos) {
        v.add(s.substring(pos, start + 1))
        newpos += 1
      } else {
        v.add(s.substring(pos, start))
      }
      pos = newpos
    }
    val remainder = s.substring(pos, len)
    v.add(remainder)
    v
  }

  /**
   * Substitutes a string for this regular expression in another string.
   * This method works like the Perl function of the same name.
   * Given a regular expression of "a*b", a String to substituteIn of
   * "aaaabfooaaabgarplyaaabwackyb" and the substitution String "-", the
   * resulting String returned by subst would be "-foo-garply-wacky-".
   * <p/>
   * It is also possible to reference the contents of a parenthesized expression
   * with $0, $1, ... $9. A regular expression of "http://[\\.\\w\\-\\?/~_@&=%]+",
   * a String to substituteIn of "visit us: http://www.apache.org!" and the
   * substitution String "&lt;a href=\"$0\"&gt;$0&lt;/a&gt;", the resulting String
   * returned by subst would be
   * "visit us: &lt;a href=\"http://www.apache.org\"&gt;http://www.apache.org&lt;/a&gt;!".
   * <p/>
   * <i>Note:</i> $0 represents the whole match.
   *
   * @param in          String to substitute within
   * @param replacement String to substitute for matches of this regular expression
   * @return The string substituteIn with zero or more occurrences of the current
   *         regular expression replaced with the substitution String (if this regular
   *         expression object doesn't match at any position, the original String is returned
   *         unchanged).
   */
  def subst(in: UnicodeString, replacement: UnicodeString): CharSequence = {
    val sb = new FastStringBuffer(in.length * 2)
    var pos = 0
    val len = in.length
    while (pos < len && `match`(in, pos)) {
      for (i ← pos until getParenStart(0)) {
        sb.appendWideChar(in.charAt(i))
      }
      if (!program.flags.isLiteral) {
        val maxCapture = getParenCount - 1
        for (i ← 0 until replacement.length) {
          var ch = replacement.charAt(i)
          if (ch == '\\') {
            ch = replacement.charAt(i)
            if (ch == '\\' || ch == '$') {
              sb.append(ch.toChar)
            } else {
              throw new RESyntaxException("Invalid escape in replacement string")
            }
          } else if (ch == '$') {
            ch = replacement.charAt(i)
            if (!(ch >= '0' && ch <= '9')) {
              throw new RESyntaxException("$ in replacement must be followed by a digit")
            }
            var n = ch - '0'
            if (maxCapture <= 9) {
              if (maxCapture >= n) {
                val captured = getParen(n)
                if (captured != null) {
                  for (j ← 0 until captured.length) {
                    sb.appendWideChar(captured.charAt(j))
                  }
                }
              } else {
              }
            } else {
              while (true) {
                if (i >= replacement.length) {
                  //break
                }
                ch = replacement.charAt(i)
                if (ch >= '0' && ch <= '9') {
                  val m = n * 10 + (ch - '0')
                  if (m > maxCapture) {
                    i -= 1
                    //break
                  } else {
                    n = m
                  }
                } else {
                  i -= 1
                  //break
                }
              }
              val captured = getParen(n)
              for (j ← 0 until captured.length) {
                sb.appendWideChar(captured.charAt(j))
              }
            }
          } else {
            sb.appendWideChar(ch)
          }
        }
      } else {
        for (i ← 0 until replacement.length) {
          sb.appendWideChar(replacement.charAt(i))
        }
      }
      var newpos = getParenEnd(0)
      if (newpos == pos) {
        newpos += 1
      }
      pos = newpos
    }
    for (i ← pos until len) {
      sb.appendWideChar(in.charAt(i))
    }
    sb.condense()
  }

  /**
   * Test whether the character at a given position is a newline
   *
   * @param i the position of the character to be tested
   * @return true if character at i-th position in the <code>search</code> string is a newline
   */
  def isNewline(i: Int): Boolean = search.charAt(i) == '\n'

  /**
   * Compares two characters.
   *
   * @param c1 first character to compare.
   * @param c2 second character to compare.
   * @return true the first character is equal to the second ignoring case.
   */
  def equalCaseBlind(c1: Int, c2: Int): Boolean = {
    if (c1 == c2) {
      return true
    }
    for (v ← CaseVariants.getCaseVariants(c2) if c1 == v) {
      return true
    }
    false
  }
}

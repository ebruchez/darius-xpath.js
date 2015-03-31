// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.LogConfiguration
import client.net.sf.saxon.ce.expr.z._
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.logging.client.LogConfiguration
import java.util.ArrayList
import java.util.Collections
import java.util.List
import RECompiler._

import scala.collection.JavaConversions._

object RECompiler {

  val NODE_NORMAL = 0

  val NODE_NULLABLE = 1

  val NODE_TOPLEVEL = 2

  val bracketUnbounded = -1

  /**
   * Test whether a character is an ASCII decimal digit
   * @param ch the character to be matched
   * @return true if the character is an ASCII digit (0-9)
   */
  private def isAsciiDigit(ch: Int): Boolean = ch >= '0' && ch <= '9'
}

class RECompiler {

  var instructions: ArrayList[Operation] = new ArrayList[Operation](20)

  var pattern: UnicodeString = _

  var len: Int = _

  var idx: Int = _

  var parens: Int = _

  var bracketMin: Int = _

  var bracketOpt: Int = _

  var isXPath: Boolean = true

  var isXPath30: Boolean = true

  var captures: IntHashSet = new IntHashSet()

  var reFlags: REFlags = _

  var warnings: List[String] = _

  /**
   * Set the regular expression flags to be used
   * @param flags the regular expression flags
   */
  def setFlags(flags: REFlags): Unit = {
    this.reFlags = flags
    isXPath = flags.isAllowsXPath20Extensions
    isXPath30 = flags.isAllowsXPath30Extensions
  }

  private def insertNode(node: Operation, insertAt: Int): Unit = {
    instructions.add(insertAt, node)
  }

  private def warning(s: String): Unit = {
    if (warnings == null) {
      warnings = new ArrayList[String](4)
    }
    warnings.add(s)
  }

  /**
   * On completion of compilation, get any warnings that were generated
   * @return the list of warning messages
   */
  def getWarnings(): List[String] = {
    if (warnings == null) {
      Collections.emptyList()
    } else {
      warnings
    }
  }

  /**
   * Appends a node to the end of a node chain
   *
   * @param node    Start of node chain to traverse
   * @param pointTo Node to have the tail of the chain point to
   */
  def setNextOfEnd(node: Int, pointTo: Int): Unit = {
    var next = instructions.get(node).next
    while (next != 0 && node < instructions.size) {
      if (node == pointTo) {
        pointTo = instructions.size
      }
      node += next
      next = instructions.get(node).next
    }
    if (node < instructions.size) {
      val offset = pointTo - node
      instructions.get(node).next = offset
    }
  }

  /**
   * Throws a new internal error exception
   *
   * @throws Error Thrown in the event of an internal error.
   */
  def internalError(): Unit = {
    throw new Error("Internal error!")
  }

  /**
   * Throws a new syntax error exception
   * @param s the error message
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def syntaxError(s: String): Unit = {
    if (LogConfiguration.loggingIsEnabled()) {
      throw new RESyntaxException(s, idx)
    } else {
      throw new RESyntaxException("", idx)
    }
  }

  /**
   * Match bracket {m,n} expression put results in bracket member variables
   *
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def bracket(): Unit = {
    if (idx >= len || pattern.charAt(idx += 1) != '{') {
      internalError()
    }
    if (idx >= len || !isAsciiDigit(pattern.charAt(idx))) {
      syntaxError("Expected digit")
    }
    val number = new StringBuffer()
    while (idx < len && isAsciiDigit(pattern.charAt(idx))) {
      number.append(pattern.charAt(idx += 1).toChar)
    }
    try {
      bracketMin = Integer.parseInt(number.toString)
    } catch {
      case e: NumberFormatException ⇒ syntaxError("Expected valid number")
    }
    if (idx >= len) {
      syntaxError("Expected comma or right bracket")
    }
    if (pattern.charAt(idx) == '}') {
      idx += 1
      bracketOpt = 0
      return
    }
    if (idx >= len || pattern.charAt(idx += 1) != ',') {
      syntaxError("Expected comma")
    }
    if (idx >= len) {
      syntaxError("Expected comma or right bracket")
    }
    if (pattern.charAt(idx) == '}') {
      idx += 1
      bracketOpt = bracketUnbounded
      return
    }
    if (idx >= len || !isAsciiDigit(pattern.charAt(idx))) {
      syntaxError("Expected digit")
    }
    number.setLength(0)
    while (idx < len && isAsciiDigit(pattern.charAt(idx))) {
      number.append(pattern.charAt(idx += 1).toChar)
    }
    try {
      bracketOpt = Integer.parseInt(number.toString) - bracketMin
    } catch {
      case e: NumberFormatException ⇒ syntaxError("Expected valid number")
    }
    if (bracketOpt < 0) {
      syntaxError("Bad range")
    }
    if (idx >= len || pattern.charAt(idx += 1) != '}') {
      syntaxError("Missing close brace")
    }
  }

  /**
   * Match an escape sequence.  Handles quoted chars and octal escapes as well
   * as normal escape characters.  Always advances the input stream by the
   * right amount. This code "understands" the subtle difference between an
   * octal escape and a backref.  You can access the type of ESC_CLASS or
   * ESC_COMPLEX or ESC_BACKREF by looking at pattern[idx - 1].
   *
   * @return an IntPredicate that matches the character or characters represented
   * by this escape sequence. For a single-character escape this must be an IntValuePredicate
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def escape(inSquareBrackets: Boolean): IntPredicate = {
    if (pattern.charAt(idx) != '\\') {
      internalError()
    }
    if (idx + 1 == len) {
      syntaxError("Escape terminates string")
    }
    idx += 2
    val escapeChar = pattern.charAt(idx - 1)
    escapeChar match {
      case 'n' ⇒ return new IntValuePredicate('\n')
      case 'r' ⇒ return new IntValuePredicate('\r')
      case 't' ⇒ return new IntValuePredicate('\t')
      case '\\' | '|' | '.' | '-' | '^' | '?' | '*' | '+' | '{' | '}' | '(' | ')' | '[' | ']' ⇒ return new IntValuePredicate(escapeChar)
      case '$' ⇒ if (isXPath) {
        return new IntValuePredicate(escapeChar)
      } else {
        syntaxError("In XSD, '$' must not be escaped")
      }
      case 's' ⇒ return Categories.ESCAPE_s
      case 'S' ⇒ return Categories.ESCAPE_S
      case 'i' ⇒ return Categories.ESCAPE_i
      case 'I' ⇒ return Categories.ESCAPE_I
      case 'c' ⇒ return Categories.ESCAPE_c
      case 'C' ⇒ return Categories.ESCAPE_C
      case 'd' ⇒ return Categories.ESCAPE_d
      case 'D' ⇒ return Categories.ESCAPE_D
      case 'w' ⇒ return Categories.ESCAPE_w
      case 'W' ⇒ return Categories.ESCAPE_W
      case 'p' | 'P' ⇒
        if (idx == len) {
          syntaxError("Expected '{' after \\" + escapeChar)
        }
        if (pattern.charAt(idx) != '{') {
          syntaxError("Expected '{' after \\" + escapeChar)
        }
        var close = pattern.indexOf('}', idx += 1)
        if (close == -1) {
          syntaxError("No closing '}' after \\" + escapeChar)
        }
        var block = pattern.substring(idx, close)
        if (block.length == 1 || block.length == 2) {
          val primary = Categories.getCategory(block.toString)
          if (primary == null) {
            syntaxError("Unknown character category " + block.toString)
          }
          idx = close + 1
          if (escapeChar == 'p') {
            return primary
          } else {
            return makeComplement(primary)
          }
        } else if (block.toString.startsWith("Is")) {
          val blockName = block.toString.substring(2)
          val uniBlock = UnicodeBlocks.getBlock(blockName)
          if (uniBlock == null) {
            warning("Unknown Unicode block: " + blockName)
            idx = close + 1
            return new IntSetPredicate(IntUniversalSet.getInstance)
          }
          idx = close + 1
          val primary = new IntSetPredicate(uniBlock)
          if (escapeChar == 'p') {
            return primary
          } else {
            return makeComplement(primary)
          }
        } else {
          syntaxError("Unknown block: " + block)
        }

      case '0' ⇒ syntaxError("Octal escapes not allowed")
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒ if (inSquareBrackets) {
        syntaxError("Backreference not allowed within character class")
      } else if (isXPath) {
        var backRef = escapeChar - '0'
        while (idx < len) {
          val c1 = "0123456789".indexOf(pattern.charAt(idx))
          if (c1 < 0) {
            //break
          } else {
            val backRef2 = backRef * 10 + c1
            if (backRef2 > parens) {
              //break
            } else {
              backRef = backRef2
              idx += 1
            }
          }
        }
        if (!captures.contains(backRef)) {
          val explanation = if (backRef > parens) "(no such group)" else "(group not yet closed)"
          syntaxError("invalid backreference \\" + backRef + " " + explanation)
        }
        return new BackReference(backRef)
      } else {
        syntaxError("digit not allowed after \\")
      }
      case _ ⇒ syntaxError("Escape character '" + escapeChar.toChar + "' not allowed")
    }
    null
  }

  /**
   * For convenience a back-reference is treated as an IntPredicate, although this a fiction
   */
  class BackReference(number: Int) extends IntValuePredicate(number)

  /**
   * Compile a character class (in square brackets)
   *
   * @return an IntPredicate that tests whether a character matches this character class
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def parseCharacterClass(): IntPredicate = {
    if (pattern.charAt(idx) != '[') {
      internalError()
    }
    if ((idx + 1) >= len || pattern.charAt(idx) == ']') {
      syntaxError("Missing ']'")
    }
    var simpleChar: Int = 0
    var positive = true
    var definingRange = false
    var rangeStart = -1
    var rangeEnd: Int = 0
    val range = new IntRangeSet()
    var addend: IntPredicate = null
    var subtrahend: IntPredicate = null
    if (thereFollows("^")) {
      if (thereFollows("^-[")) {
        syntaxError("Nothing before subtraction operator")
      } else if (thereFollows("^]")) {
        syntaxError("Empty negative character group")
      } else {
        positive = false
        idx += 1
      }
    } else if (thereFollows("-[")) {
      syntaxError("Nothing before subtraction operator")
    }
    while (idx < len && pattern.charAt(idx) != ']') {
      val ch = pattern.charAt(idx)
      simpleChar = -1
      ch match {
        case '[' ⇒ syntaxError("Unescaped '[' within square brackets")
        case '\\' ⇒ {
          val cc = escape(true)
          if (cc.isInstanceOf[IntValuePredicate]) {
            simpleChar = cc.asInstanceOf[IntValuePredicate].getTarget
            //break
          } else {
            if (definingRange) {
              syntaxError("Multi-character escape cannot follow '-'")
            } else addend = if (addend == null) cc else makeUnion(addend, cc)
            //continue
          }
        }
        case '-' ⇒ if (thereFollows("-[")) {
          idx += 1
          subtrahend = parseCharacterClass()
          if (!thereFollows("]")) {
            syntaxError("Expected closing ']' after subtraction")
          }
        } else if (thereFollows("-]")) {
          simpleChar = '-'
          idx += 1
        } else if (rangeStart >= 0) {
          definingRange = true
          idx += 1
          //continue
        } else if (definingRange) {
          syntaxError("Bad range")
        } else if (thereFollows("--") && !thereFollows("--[")) {
          syntaxError("Unescaped hyphen as start of range")
        } else {
          simpleChar = '-'
          idx += 1
        }
        case _ ⇒
          simpleChar = ch
          idx += 1

      }
      if (definingRange) {
        rangeEnd = simpleChar
        if (rangeStart > rangeEnd) {
          syntaxError("Bad character range: start > end")
        }
        range.addRange(rangeStart, rangeEnd)
        if (reFlags.isCaseIndependent) {
          if (rangeStart == 'a' && rangeEnd == 'z') {
            range.addRange('A', 'Z')
            for (v ← 0 until CaseVariants.ROMAN_VARIANTS.length) {
              range.add(CaseVariants.ROMAN_VARIANTS(v))
            }
          } else if (rangeStart == 'A' && rangeEnd == 'Z') {
            range.addRange('a', 'z')
            for (v ← 0 until CaseVariants.ROMAN_VARIANTS.length) {
              range.add(CaseVariants.ROMAN_VARIANTS(v))
            }
          } else {
            var k = rangeStart
            while (k <= rangeEnd) {
              val variants = CaseVariants.getCaseVariants(k)
              for (variant ← variants) {
                range.add(variant)
              }
              k += 1
            }
          }
        }
        definingRange = false
        rangeStart = -1
      } else {
        if (thereFollows("-")) {
          if (thereFollows("-[")) {
            range.add(simpleChar)
          } else if (thereFollows("-]")) {
            range.add(simpleChar)
          } else if (thereFollows("--[")) {
            range.add(simpleChar)
          } else if (thereFollows("--")) {
            syntaxError("Unescaped hyphen cannot act as end of range")
          } else {
            rangeStart = simpleChar
          }
        } else {
          range.add(simpleChar)
          if (reFlags.isCaseIndependent) {
            val variants = CaseVariants.getCaseVariants(simpleChar)
            for (variant ← variants) {
              range.add(variant)
            }
          }
        }
      }
    }
    if (idx == len) {
      syntaxError("Unterminated character class")
    }
    idx += 1
    var result = new IntSetPredicate(range)
    if (addend != null) {
      result = makeUnion(result, addend)
    }
    if (!positive) {
      result = makeComplement(result)
    }
    if (subtrahend != null) {
      result = makeDifference(result, subtrahend)
    }
    result
  }

  /**
   * Test whether the string starting at the current position is equal to some specified string
   * @param s the string being tested
   * @return true if the specified string is present
   */
  private def thereFollows(s: String): Boolean = {
    idx + s.length <= len && 
      (pattern.substring(idx, idx + s.length).toString == s)
  }

  /**
   * Make the union of two IntPredicates (matches if p1 matches or p2 matches)
   * @param p1 the first
   * @param p2 the second
   * @return the result
   */
  private def makeUnion(p1: IntPredicate, p2: IntPredicate): IntPredicate = {
    if (p1.isInstanceOf[IntSetPredicate] && p1.asInstanceOf[IntSetPredicate].getIntSet.isEmpty) {
      return p2
    }
    if (p2.isInstanceOf[IntSetPredicate] && p2.asInstanceOf[IntSetPredicate].getIntSet.isEmpty) {
      return p1
    }
    new IntUnionPredicate(p1, p2)
  }

  /**
   * Make the difference of two IntPredicates (matches if p1 matches and p2 does not match)
   * @param p1 the first
   * @param p2 the second
   * @return the result
   */
  private def makeDifference(p1: IntPredicate, p2: IntPredicate): IntPredicate = new IntExceptPredicate(p1, p2)

  /**
   * Make the complement of an IntPredicate (matches if p1 does not match)
   * @param p1 the operand
   * @return the result
   */
  private def makeComplement(p1: IntPredicate): IntPredicate = {
    if (p1.isInstanceOf[IntComplementPredicate]) {
      p1.asInstanceOf[IntComplementPredicate].getOperand
    } else {
      new IntComplementPredicate(p1)
    }
  }

  private def emitCharacterClass(range: IntPredicate): Int = {
    val node = new Operation.OpCharClass()
    node.predicate = range
    appendNode(node)
  }

  /**
   * Absorb an atomic character string.  This method is a little tricky because
   * it can un-include the last character of string if a quantifier operator follows.
   * This is correct because *+? have higher precedence than concatentation (thus
   * ABC* means AB(C*) and NOT (ABC)*).
   *
   * @return Index of new atom node
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def atom(): Int = {
    val node = new Operation.OpAtom()
    var lenAtom = 0
    val fsb = new FastStringBuffer(FastStringBuffer.SMALL)
    atomLoop: while (idx < len) {
      if ((idx + 1) < len) {
        var c = pattern.charAt(idx + 1)
        if (pattern.charAt(idx) == '\\') {
          val idxEscape = idx
          escape(false)
          if (idx < len) {
            c = pattern.charAt(idx)
          }
          idx = idxEscape
        }
        c match {
          case '{' | '?' | '*' | '+' ⇒ if (lenAtom != 0) {
            //break
          }
        }
      }
      pattern.charAt(idx) match {
        case ']' | '.' | '[' | '(' | ')' | '|' ⇒ //break
        case '{' | '?' | '*' | '+' ⇒ if (lenAtom == 0) {
          syntaxError("No expression before quantifier")
        }
        case '\\' ⇒ {
          val idxBeforeEscape = idx
          val charClass = escape(false)
          if (charClass.isInstanceOf[BackReference] || !charClass.isInstanceOf[IntValuePredicate]) {
            idx = idxBeforeEscape
            //break
          }
          fsb.appendWideChar(charClass.asInstanceOf[IntValuePredicate].getTarget)
          lenAtom += 1
          //break
        }
        case '^' | '$' ⇒ if (isXPath) {
          //break
        }
        case _ ⇒
          fsb.appendWideChar(pattern.charAt(idx += 1))
          lenAtom += 1

      }
    }
    if (fsb.length == 0) {
      internalError()
    }
    node.atom = GeneralUnicodeString.makeUnicodeString(fsb.condense())
    appendNode(node)
  }

  private def appendNode(node: Operation): Int = {
    instructions.add(node)
    instructions.size - 1
  }

  /**
   * Match a terminal node.
   *
   * @param flags Flags
   * @return Index of terminal node (closeable)
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def terminal(flags: Array[Int]): Int = pattern.charAt(idx) match {
    case '$' ⇒ if (isXPath) {
      idx += 1
      val eol = new Operation.OpEOL()
      appendNode(eol)
    }
    case '^' ⇒ if (isXPath) {
      idx += 1
      val bol = new Operation.OpBOL()
      appendNode(bol)
    }
    case '.' ⇒
      idx += 1
      var predicate: IntPredicate = null
      predicate = if (reFlags.isSingleLine) new IntPredicate() {

        def matches(value: Int): Boolean = true
      } else new IntPredicate() {

        def matches(value: Int): Boolean = value != '\n' && value != '\r'
      }
      var dot = new Operation.OpCharClass()
      dot.predicate = predicate
      appendNode(dot)

    case '[' ⇒
      var range = parseCharacterClass()
      var cc = new Operation.OpCharClass()
      cc.predicate = range
      appendNode(cc)

    case '(' ⇒ expr(flags)
    case ')' ⇒ syntaxError("Unexpected close paren")
    case '|' ⇒ internalError()
    case ']' ⇒ syntaxError("Mismatched class")
    case 0 ⇒ syntaxError("Unexpected end of input")
    case '?' | '+' | '{' | '*' ⇒ syntaxError("No expression before quantifier")
    case '\\' ⇒ {
      val idxBeforeEscape = idx
      val esc = escape(false)
      if (esc.isInstanceOf[BackReference]) {
        val backreference = esc.asInstanceOf[BackReference].getTarget
        if (parens <= backreference) {
          syntaxError("Bad backreference")
        }
        flags(0) |= NODE_NULLABLE
        val back = new Operation.OpBackReference()
        back.groupNr = backreference
        appendNode(back)
      } else if (esc.isInstanceOf[IntSingletonSet]) {
        idx = idxBeforeEscape
        flags(0) &= ~NODE_NULLABLE
      } else {
        flags(0) &= ~NODE_NULLABLE
        emitCharacterClass(esc)
      }
    }
  }

  /**
   * Compile a piece consisting of an atom and optional quantifier
   *
   * @param flags Flags passed by reference
   * @return Index of resulting instruction
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def piece(flags: Array[Int]): Int = {
    val idxBeforeTerminal = idx
    val terminalFlags = Array(NODE_NORMAL)
    val ret = terminal(terminalFlags)
    flags(0) |= terminalFlags(0)
    if (idx >= len) {
      return ret
    }
    var greedy = true
    var quantifierType = pattern.charAt(idx)
    quantifierType match {
      case '?' | '*' ⇒ flags(0) |= NODE_NULLABLE
      case '+' ⇒ idx += 1
      case '{' ⇒
        if (quantifierType == '{') {
          bracket()
        }
        var op = instructions.get(ret)
        if (op.isInstanceOf[Operation.OpBOL] || op.isInstanceOf[Operation.OpEOL]) {
          if (quantifierType == '?' || quantifierType == '*' || (quantifierType == '{' && bracketMin == 0)) {
            instructions.set(ret, new Operation.OpNothing())
          } else {
            quantifierType = 0
          }
        }
        if ((terminalFlags(0) & NODE_NULLABLE) != 0) {
          if (quantifierType == '?') {
            quantifierType = 0
          } else if (quantifierType == '+') {
            quantifierType = '*'
          } else if (quantifierType == '{') {
            quantifierType = '*'
          }
        }

    }
    if (idx < len && pattern.charAt(idx) == '?') {
      if (!isXPath) {
        syntaxError("Reluctant quantifiers are not allowed in XSD")
      }
      idx += 1
      greedy = false
    }
    if (greedy) quantifierType match {
      case '{' ⇒ {
        val bracketEnd = idx
        val bracketMin = this.bracketMin
        val bracketOpt = this.bracketOpt
        var pos = ret
        for (c ← 0 until bracketMin) {
          idx = idxBeforeTerminal
          setNextOfEnd(pos, pos = terminal(terminalFlags))
        }
        if (bracketOpt == bracketUnbounded) {
          idx = bracketEnd
          val op = new Operation.OpStar()
          insertNode(op, pos)
          setNextOfEnd(pos + 1, pos)
          //break
        } else if (bracketOpt > 0) {
          val opt = Array.ofDim[Int](bracketOpt + 1)
          var op = new Operation.OpMaybe()
          insertNode(op, pos)
          opt(0) = pos
          for (c ← 1 until bracketOpt) {
            op = new Operation.OpMaybe()
            opt(c) = appendNode(op)
            idx = idxBeforeTerminal
            terminal(terminalFlags)
          }
          val end = opt(bracketOpt) = appendNode(new Operation.OpNothing())
          for (c ← 0 until bracketOpt) {
            setNextOfEnd(opt(c), end)
            setNextOfEnd(opt(c) + 1, opt(c + 1))
          }
        } else {
          while (instructions.size > pos) {
            instructions.remove(instructions.size - 1)
          }
          val nothing = new Operation.OpNothing()
          appendNode(nothing)
        }
        idx = bracketEnd
        //break
      }
      case '?' ⇒ {
        val maybe = new Operation.OpMaybe()
        insertNode(maybe, ret)
        val nothing = new Operation.OpNothing()
        val n = appendNode(nothing)
        setNextOfEnd(ret, n)
        setNextOfEnd(ret + 1, n)
        //break
      }
      case '*' ⇒ {
        val star = new Operation.OpStar()
        insertNode(star, ret)
        setNextOfEnd(ret + 1, ret)
        //break
      }
      case '+' ⇒ {
        val continu = new Operation.OpContinue()
        insertNode(continu, ret)
        val plus = new Operation.OpPlus()
        val n = appendNode(plus)
        setNextOfEnd(ret + 1, n)
        setNextOfEnd(n, ret)
        //break
      }
    } else quantifierType match {
      case '?' ⇒ {
        val reluctantMaybe = new Operation.OpReluctantMaybe()
        insertNode(reluctantMaybe, ret)
        val n = appendNode(new Operation.OpNothing())
        setNextOfEnd(ret, n)
        setNextOfEnd(ret + 1, n)
        //break
      }
      case '*' ⇒ {
        val reluctantStar = new Operation.OpReluctantStar()
        insertNode(reluctantStar, ret)
        setNextOfEnd(ret + 1, ret)
        //break
      }
      case '+' ⇒ {
        insertNode(new Operation.OpContinue(), ret)
        val n = appendNode(new Operation.OpReluctantPlus())
        setNextOfEnd(n, ret)
        setNextOfEnd(ret + 1, n)
        //break
      }
      case '{' ⇒ {
        val bracketEnd = idx
        val bracketMin = this.bracketMin
        val bracketOpt = this.bracketOpt
        var pos = ret
        for (c ← 0 until bracketMin) {
          idx = idxBeforeTerminal
          setNextOfEnd(pos, pos = terminal(terminalFlags))
        }
        if (bracketOpt == bracketUnbounded) {
          idx = bracketEnd
          insertNode(new Operation.OpReluctantStar(), pos)
          setNextOfEnd(pos + 1, pos)
          //break
        } else if (bracketOpt > 0) {
          val opt = Array.ofDim[Int](bracketOpt + 1)
          insertNode(new Operation.OpReluctantMaybe(), pos)
          opt(0) = pos
          for (c ← 1 until bracketOpt) {
            opt(c) = appendNode(new Operation.OpReluctantMaybe())
            idx = idxBeforeTerminal
            terminal(terminalFlags)
          }
          val end = opt(bracketOpt) = appendNode(new Operation.OpNothing())
          for (c ← 0 until bracketOpt) {
            setNextOfEnd(opt(c), end)
            setNextOfEnd(opt(c) + 1, opt(c + 1))
          }
        } else {
          while (instructions.size > pos) {
            instructions.remove(instructions.size - 1)
          }
          appendNode(new Operation.OpNothing())
        }
        idx = bracketEnd
        //break
      }
    }
    ret
  }

  /**
   * Compile body of one branch of an or operator (implements concatenation)
   *
   * @param compilerFlags Flags passed by reference
   * @return Pointer to first node in the branch
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def branch(compilerFlags: Array[Int]): Int = {
    var node: Int = 0
    var ret = -1
    var chain = -1
    val quantifierFlags = Array.ofDim[Int](1)
    var nullable = true
    while (idx < len && pattern.charAt(idx) != '|' && pattern.charAt(idx) != ')') {
      quantifierFlags(0) = NODE_NORMAL
      node = piece(quantifierFlags)
      if (quantifierFlags(0) == NODE_NORMAL) {
        nullable = false
      }
      if (chain != -1) {
        setNextOfEnd(chain, node)
      }
      chain = node
      if (ret == -1) {
        ret = node
      }
    }
    if (ret == -1) {
      val nothing = new Operation.OpNothing()
      ret = appendNode(nothing)
    }
    if (nullable) {
      compilerFlags(0) |= NODE_NULLABLE
    }
    ret
  }

  /**
   * Compile an expression with possible parens around it.  Paren matching
   * is done at this level so we can tie the branch tails together.
   *
   * @param compilerFlags Flag value passed by reference
   * @return Node index of expression in instruction array
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   */
  def expr(compilerFlags: Array[Int]): Int = {
    var paren = -1
    var ret = -1
    val closeParens = parens
    if ((compilerFlags(0) & NODE_TOPLEVEL) == 0 && pattern.charAt(idx) == '(') {
      if (idx + 2 < len && pattern.charAt(idx + 1) == '?' && pattern.charAt(idx + 2) == ':') {
        if (!isXPath30) {
          syntaxError("Non-capturing groups allowed only in XPath3.0")
        }
        paren = 2
        idx += 3
        ret = appendNode(new Operation.OpOpenCluster())
      } else {
        paren = 1
        idx += 1
        ret = appendNode(new Operation.OpOpen(parens += 1))
      }
    }
    compilerFlags(0) &= ~NODE_TOPLEVEL
    var open = false
    var branch = branch(compilerFlags)
    if (ret == -1) {
      ret = branch
    } else {
      setNextOfEnd(ret, branch)
    }
    while (idx < len && pattern.charAt(idx) == '|') {
      if (!open) {
        val op = new Operation.OpBranch()
        insertNode(op, branch)
        open = true
      }
      idx += 1
      setNextOfEnd(branch, branch = appendNode(new Operation.OpBranch()))
      branch(compilerFlags)
    }
    var end: Int = 0
    if (paren > 0) {
      if (idx < len && pattern.charAt(idx) == ')') {
        idx += 1
      } else {
        syntaxError("Missing close paren")
      }
      if (paren == 1) {
        end = appendNode(new Operation.OpClose(closeParens))
        captures.add(closeParens)
      } else {
        end = appendNode(new Operation.OpCloseCluster())
      }
    } else {
      end = appendNode(new Operation.OpEndProgram())
    }
    setNextOfEnd(ret, end)
    var currentNode = ret
    var nextNodeOffset = instructions.get(currentNode).next
    while (nextNodeOffset != 0 && currentNode < instructions.size) {
      if (instructions.get(currentNode).isInstanceOf[Operation.OpBranch]) {
        setNextOfEnd(currentNode + 1, end)
      }
      nextNodeOffset = instructions.get(currentNode).next
      currentNode += nextNodeOffset
    }
    ret
  }

  /**
   * Compiles a regular expression pattern into a program runnable by the pattern
   * matcher class 'RE'.
   *
   * @param pattern Regular expression pattern to compile (see RECompiler class
   *                for details).
   * @return A compiled regular expression program.
   * @throws RESyntaxException Thrown if the regular expression has invalid syntax.
   * @see RECompiler
   * @see REMatcher
   */
  def compile(pattern: UnicodeString): REProgram = {
    this.pattern = pattern
    len = pattern.length
    idx = 0
    parens = 1
    var nullable = false
    if (reFlags.isLiteral) {
      val ret = literalAtom()
      val endNode = new Operation.OpEndProgram()
      val end = appendNode(endNode)
      setNextOfEnd(ret, end)
    } else {
      if (reFlags.isAllowWhitespace) {
        val sb = new FastStringBuffer(pattern.length)
        var nesting = 0
        var astral = false
        var escaped = false
        for (i ← 0 until pattern.length) {
          val ch = pattern.charAt(i)
          if (ch > 65535) {
            astral = true
          }
          if (ch == '\\' && !escaped) {
            escaped = true
            sb.appendWideChar(ch)
          } else if (ch == '[' && !escaped) {
            nesting += 1
            escaped = false
            sb.appendWideChar(ch)
          } else if (ch == ']' && !escaped) {
            nesting -= 1
            escaped = false
            sb.appendWideChar(ch)
          } else if (nesting == 0 && Whitespace.isWhitespace(ch)) {
          } else {
            escaped = false
            sb.appendWideChar(ch)
          }
        }
        pattern = if (astral) new GeneralUnicodeString(sb) else new BMPString(sb)
        this.pattern = pattern
        this.len = pattern.length
      }
      val compilerFlags = Array(NODE_TOPLEVEL)
      expr(compilerFlags)
      nullable = (compilerFlags(0) & NODE_NULLABLE) != 0
      if (idx != len) {
        if (pattern.charAt(idx) == ')') {
          syntaxError("Unmatched close paren")
        }
        syntaxError("Unexpected input remains")
      }
    }
    val ops = Array.ofDim[Operation](instructions.size)
    for (i ← 0 until instructions.size) {
      val op = instructions.get(i)
      if (op.next == 0) {
        op.next = -1
      } else {
        op.next += i
      }
      ops(i) = op
    }
    val program = new REProgram(ops, parens, reFlags)
    if (reFlags.isDebug) {
      program.display(System.err)
    }
    program.setNullable(nullable)
    program
  }

  /**
   * Process a "regular expression" with the q flag set. This is simply handled as an atom, where
   * no characters are treated as special (i.e. all are treated as if escaped)
   *
   * @return Index of new atom node
   */
  def literalAtom(): Int = {
    val node = new Operation.OpAtom()
    node.atom = pattern
    appendNode(node)
  }

  /**
   * Return a string describing a (possibly unprintable) character.
   *
   * @param c Character to convert to a printable representation
   * @return String representation of character
   */
  def charToString(c: Char): String = {
    if (c < ' ' || c > 127) {
      return "\\" + c.toInt
    }
    String.valueOf(c)
  }
}

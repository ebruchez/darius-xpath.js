package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.expr.z.IntPredicate
import Operation._
//remove if not needed
import scala.collection.JavaConversions._

object Operation {

  val ACTION_ADVANCE_TO_NEXT = 1

  val ACTION_RETURN = 2

  val ACTION_ADVANCE_TO_FOLLOWING = 3

  val ACTION_ADVANCE_TO_NEXT_NEXT = 4

  /**
   * End of program
   */
  class OpEndProgram extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.anchoredMatch) {
        (if (matcher.search.isEnd(idx)) idx else -1)
      } else {
        matcher.setParenEnd(0, idx)
        idx
      }
    }

    def nextAction(idx: Int): Int = ACTION_RETURN

    override def toString(): String = "END"
  }

  /**
   * Beginning of Line (^)
   */
  class OpBOL extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (idx != 0) {
        if (matcher.program.flags.isMultiLine) {
          if (matcher.isNewline(idx - 1)) {
            return idx
          }
        }
        return -1
      }
      idx
    }

    override def toString(): String = "BOL"
  }

  /**
   * End of Line ($)
   */
  class OpEOL extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      val search = matcher.search
      if (matcher.program.flags.isMultiLine) {
        if (search.isEnd(0) || search.isEnd(idx) || matcher.isNewline(idx)) {
          idx
        } else {
          -1
        }
      } else {
        if (search.isEnd(0) || search.isEnd(idx) || (matcher.isNewline(idx) && search.isEnd(idx + 1))) {
          idx
        } else {
          -1
        }
      }
    }

    override def toString(): String = "EOL"
  }

  /**
   * Choice (|)
   */
  class OpBranch extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      var idxNew: Int = 0
      do {
        if ((idxNew = matcher.matchNodes(node + 1, idx)) != -1) {
          return idxNew
        }
        node = matcher.instructions(node).next
      } while (node != -1 && 
        (matcher.program.instructions(node).isInstanceOf[Operation.OpBranch]));
      -1
    }

    def nextAction(idx: Int): Int = ACTION_RETURN

    override def toString(): String = "BRANCH"
  }

  /**
   * Atom
   */
  class OpAtom extends Operation {

    var atom: UnicodeString = _

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      val search = matcher.search
      if (search.isEnd(idx)) {
        return -1
      }
      if (search.isEnd(atom.length + idx - 1)) {
        return -1
      }
      if (matcher.program.flags.isCaseIndependent) {
        for (i <- 0 until atom.length if !matcher.equalCaseBlind(search.charAt(idx += 1), atom.charAt(i))) {
          return -1
        }
      } else {
        for (i <- 0 until atom.length if search.charAt(idx += 1) != atom.charAt(i)) {
          return -1
        }
      }
      idx
    }

    override def toString(): String = "ATOM \"" + atom.toString + "\""
  }

  /**
   * Star quantifier
   */
  class OpStar extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.beenHereBefore(idx, node)) {
        return -1
      }
      matcher.matchNodes(node + 1, idx)
    }

    def nextAction(idx: Int): Int = {
      if (idx == -1) {
        ACTION_ADVANCE_TO_NEXT
      } else {
        ACTION_RETURN
      }
    }

    override def toString(): String = "STAR"
  }

  /**
   * "Confident Star" quantifier: used when there is no ambiguity about the ending condition,
   * and therefore no need to backtrack. This means we can use iteration rather than recursion,
   * eliminating the risk of stack overflow.
   */
  class OpConfidentStar extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.beenHereBefore(idx, node)) {
        return -1
      }
      var newIdx: Int = 0
      val term = matcher.instructions(node + 1)
      while (true) {
        newIdx = term.exec(matcher, node + 1, idx)
        if (newIdx == -1) {
          idx
        } else {
          idx = newIdx
        }
      }
    }

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_NEXT

    override def toString(): String = "CONFIDENT_STAR"
  }

  /**
   * Plus quantifier
   */
  class OpPlus extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = matcher.matchNodes(next, idx)

    def nextAction(idx: Int): Int = {
      if (idx == -1) {
        ACTION_ADVANCE_TO_NEXT_NEXT
      } else {
        ACTION_RETURN
      }
    }

    override def toString(): String = "PLUS"
  }

  /**
   * "Confident Plus" quantifier: used when there is no ambiguity about the ending condition,
   * and therefore no need to backtrack. This means we can use iteration rather than recursion,
   * eliminating the risk of stack overflow.
   */
  class OpConfidentPlus extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.beenHereBefore(idx, node)) {
        return -1
      }
      var newIdx: Int = 0
      val term = matcher.instructions(node - 1)
      while (true) {
        newIdx = term.exec(matcher, node - 1, idx)
        if (newIdx == -1) {
          idx
        } else {
          idx = newIdx
        }
      }
    }

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_NEXT

    override def toString(): String = "CONFIDENT_PLUS"
  }

  /**
   * Maybe (question-mark) quantifier
   */
  class OpMaybe extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.beenHereBefore(idx, node)) {
        return -1
      }
      matcher.matchNodes(node + 1, idx)
    }

    def nextAction(idx: Int): Int = {
      if (idx == -1) {
        ACTION_ADVANCE_TO_NEXT
      } else {
        ACTION_RETURN
      }
    }

    override def toString(): String = "MAYBE"
  }

  /**
   * Open paren (captured group)
   */
  class OpOpen(group: Int) extends Operation {

    var groupNr: Int = group

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if ((matcher.program.optimizationFlags & REProgram.OPT_HASBACKREFS) != 
        0) {
        matcher.startBackref(groupNr) = idx
      }
      val idxNew = matcher.matchNodes(next, idx)
      if (idxNew != -1) {
        if (groupNr >= matcher.parenCount) {
          matcher.parenCount = groupNr + 1
        }
        if (matcher.getParenStart(groupNr) == -1) {
          matcher.setParenStart(groupNr, idx)
        }
      }
      idxNew
    }

    def nextAction(idx: Int): Int = ACTION_RETURN

    override def toString(): String = "OPEN_GROUP " + groupNr
  }

  /**
   * Open non-capturing paren
   */
  class OpOpenCluster extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = idx

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_NEXT

    override def toString(): String = "OPEN_CLUSTER"
  }

  /**
   * Close paren (captured group)
   */
  class OpClose(var groupNr: Int) extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if ((matcher.program.optimizationFlags & REProgram.OPT_HASBACKREFS) != 
        0) {
        matcher.endBackref(groupNr) = idx
      }
      val idxNew = matcher.matchNodes(next, idx)
      if (idxNew != -1) {
        if (groupNr >= matcher.parenCount) {
          matcher.parenCount = groupNr + 1
        }
        if (matcher.getParenEnd(groupNr) == -1) {
          matcher.setParenEnd(groupNr, idx)
        }
      }
      idxNew
    }

    def nextAction(idx: Int): Int = ACTION_RETURN

    override def toString(): String = "CLOSE_GROUP " + groupNr
  }

  /**
   * Close non-capturing group
   */
  class OpCloseCluster extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = idx

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_NEXT

    override def toString(): String = "CLOSE_CLUSTER"
  }

  /**
   * Back-reference
   */
  class OpBackReference extends Operation {

    var groupNr: Int = _

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      val s = matcher.startBackref(groupNr)
      val e = matcher.endBackref(groupNr)
      if (s == -1 || e == -1) {
        return -1
      }
      if (s == e) {
        return idx
      }
      val l = e - s
      val search = matcher.search
      if (search.isEnd(idx + l - 1)) {
        return -1
      }
      if (matcher.program.flags.isCaseIndependent) {
        for (i <- 0 until l if !matcher.equalCaseBlind(search.charAt(idx += 1), search.charAt(s + i))) {
          return -1
        }
      } else {
        for (i <- 0 until l if search.charAt(idx += 1) != search.charAt(s + i)) {
          return -1
        }
      }
      idx
    }

    override def toString(): String = "BACKREF " + groupNr
  }

  /**
   * Goto specified instruction
   */
  class OpGoTo extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = idx

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_NEXT

    override def toString(): String = "GOTO"
  }

  /**
   * Match empty string
   */
  class OpNothing extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = idx

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_NEXT

    override def toString(): String = "NOTHING"
  }

  /**
   * Continue to the following instruction (ignore 'next')
   */
  class OpContinue extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = idx

    def nextAction(idx: Int): Int = ACTION_ADVANCE_TO_FOLLOWING

    override def toString(): String = "CONTINUE"
  }

  /**
   * Reluctant star operator
   */
  class OpReluctantStar extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.beenHereBefore(idx, node)) {
        return -1
      }
      val idxNew = matcher.matchNodes(next, idx)
      if (idxNew != -1) {
        return idxNew
      }
      matcher.matchNodes(node + 1, next, idx)
    }

    def nextAction(idx: Int): Int = ACTION_RETURN

    override def toString(): String = "RELUCTANT_STAR"
  }

  /**
   * Reluctant plus operator
   */
  class OpReluctantPlus extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      matcher.matchNodes(matcher.instructions(next).next, idx)
    }

    def nextAction(idx: Int): Int = {
      if (idx == -1) {
        ACTION_ADVANCE_TO_NEXT
      } else {
        ACTION_RETURN
      }
    }

    override def toString(): String = "RELUCTANT_PLUS"
  }

  /**
   * Reluctant maybe operator
   */
  class OpReluctantMaybe extends Operation {

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      if (matcher.beenHereBefore(idx, node)) {
        return -1
      }
      val idxNew = matcher.matchNodes(next, idx)
      if (idxNew != -1) {
        return idxNew
      }
      matcher.matchNodes(node + 1, next, idx)
    }

    def nextAction(idx: Int): Int = ACTION_RETURN

    override def toString(): String = "RELUCTANT_MAYBE"
  }

  /**
   * Character class: match any one of a set of characters
   */
  class OpCharClass extends Operation {

    var predicate: IntPredicate = _

    def exec(matcher: REMatcher, node: Int, idx: Int): Int = {
      val search = matcher.search
      if (search.isEnd(idx)) {
        return -1
      }
      if (!predicate.matches(search.charAt(idx))) {
        return -1
      }
      idx + 1
    }

    override def toString(): String = {
      "CHAR_CLASS (" + predicate.getClass + ") "
    }
  }
}

/**
 * Represents an operation or instruction in the regular expression program. The class Operation
 * is abstract, and has concrete subclasses for each kind of operation/instruction
 */
abstract class Operation {

  var next: Int = _

  /**
   * Execute the operation
   * @param matcher the REMatcher
   * @param node the program node containing this operation
   * @param idx the current position in the input string
   * @return >=0: matching succeeded, returns new position in input string.
   * -1: matching failed: return to caller.
   */
  def exec(matcher: REMatcher, node: Int, idx: Int): Int

  /**
   * Determine the action to take after calling exec()
   * @param idx the value returned by exec()
   * @return one of the values ACTION_RETURN, ACTION_ADVANCE_TO_NEXT, ...
   */
  def nextAction(idx: Int): Int = {
    if (idx == -1) {
      ACTION_RETURN
    } else {
      ACTION_ADVANCE_TO_NEXT
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import org.orbeon.darius.xpath.expr.z._
import java.io.PrintStream
import REProgram._

import scala.collection.JavaConversions._

object REProgram {

  val OPT_HASBACKREFS = 1

  val OPT_HASBOL = 2
}

/**
 * A class that holds compiled regular expressions.
 *
 * @see REMatcher
 * @see RECompiler
 *
 * @author <a href="mailto:jonl@muppetlabs.com">Jonathan Locke</a>
 * @version $Id: REProgram.java 518156 2007-03-14 14:31:26Z vgritsenko $
 */
class REProgram(instructions: Array[Operation], parens: Int, var flags: REFlags) {

  var instructions: Array[Operation] = _

  var prefix: UnicodeString = _

  var optimizationFlags: Int = _

  var maxParens: Int = parens

  var nullable: Boolean = false

  setInstructions(instructions)

  /**
   * Sets a new regular expression program to run.  It is this method which
   * performs any special compile-time search optimizations.  Currently only
   * two optimizations are in place - one which checks for backreferences
   * (so that they can be lazily allocated) and another which attempts to
   * find an prefix anchor string so that substantial amounts of input can
   * potentially be skipped without running the actual program.
   * @param instructions Program instruction buffer
   */
  private def setInstructions(instructions: Array[Operation]): Unit = {
    this.instructions = instructions
    this.optimizationFlags = 0
    this.prefix = null
    if (instructions != null && instructions.length != 0) {
      if (instructions(0).isInstanceOf[Operation.OpAtom]) {
        prefix = instructions(0).asInstanceOf[Operation.OpAtom].atom
      }
      if (instructions(0).isInstanceOf[Operation.OpBranch]) {
        val next = instructions(0).next
        if (instructions(next).isInstanceOf[Operation.OpEndProgram]) {
          val nextOp = instructions(1)
          if (nextOp.isInstanceOf[Operation.OpAtom]) {
            this.prefix = nextOp.asInstanceOf[Operation.OpAtom].atom
          } else if (nextOp.isInstanceOf[Operation.OpBOL]) {
            this.optimizationFlags |= OPT_HASBOL
          }
        }
      }
      for (op ← instructions if op.isInstanceOf[Operation.OpBackReference]) {
        optimizationFlags |= OPT_HASBACKREFS
        //break
      }
      val caseBlind = flags.isCaseIndependent
      for (i ← 0 until instructions.length) {
        val op = instructions(i)
        if (op.isInstanceOf[Operation.OpStar] && op.next == i + 2 && 
          (instructions(i + 1).isInstanceOf[Operation.OpAtom] || 
          instructions(i + 1).isInstanceOf[Operation.OpCharClass])) {
          if (noAmbiguity(instructions(i + 1), instructions(op.next), caseBlind)) {
            instructions(i) = new Operation.OpConfidentStar()
            instructions(i).next = op.next
          }
        } else if (op.isInstanceOf[Operation.OpPlus] && op.next == i - 2 && 
          (instructions(i - 1).isInstanceOf[Operation.OpAtom] || 
          instructions(i - 1).isInstanceOf[Operation.OpCharClass]) && 
          (instructions(i - 2).next == i + 1)) {
          if (noAmbiguity(instructions(i - 1), instructions(i + 1), caseBlind)) {
            instructions(i) = new Operation.OpConfidentPlus()
            instructions(i).next = i + 1
          }
        }
      }
    }
  }

  /**
   * Ask whether the regular expression matches a zero length string
   * @return true if the regex matches a zero length string
   */
  def isNullable(): Boolean = nullable

  /**
   * Say whether the regular expression matches a zero length string
   * @param nullable true if the regex matches a zero length string
   */
  def setNullable(nullable: Boolean): Unit = {
    this.nullable = nullable
  }

  /**
   * Returns a copy of the prefix of current regular expression program
   * in a character array.  If there is no prefix, or there is no program
   * compiled yet, <code>getPrefix</code> will return null.
   * @return A copy of the prefix of current compiled RE program
   */
  def getPrefix(): UnicodeString = prefix

  /**
   * Output a human-readable printout of the program
   */
  def display(out: PrintStream): Unit = {
    for (i ← 0 until instructions.length) {
      val nextOffset = instructions(i).next
      out.println(i + ". " + instructions(i).toString + 
        (if (nextOffset == -1) "" else ", next = " + nextOffset))
    }
  }

  /**
   * Determine that there is no ambiguity between two branches, that is, if one of them matches then the
   * other cannot possibly match. (This is for optimization, so it does not have to detect all cases; but
   * if it returns true, then the result must be dependable.)
   * @return true if it can be established that there is no input sequence that will match both instructions
   */
  def noAmbiguity(op0: Operation, op1: Operation, caseBlind: Boolean): Boolean = {
    if (op1.isInstanceOf[Operation.OpClose] || op1.isInstanceOf[Operation.OpCloseCluster]) {
      op1 = instructions(op1.next)
    }
    if (op1.isInstanceOf[Operation.OpEndProgram] || op1.isInstanceOf[Operation.OpBOL] || 
      op1.isInstanceOf[Operation.OpEOL]) {
      return true
    }
    var set0: IntSet = null
    if (op0.isInstanceOf[Operation.OpAtom]) {
      set0 = getInitialChars(op0.asInstanceOf[Operation.OpAtom], caseBlind)
    } else {
      val ip0 = op0.asInstanceOf[Operation.OpCharClass].predicate
      if (ip0.isInstanceOf[IntSetPredicate]) {
        set0 = ip0.asInstanceOf[IntSetPredicate].getIntSet
      } else if (ip0.isInstanceOf[IntValuePredicate]) {
        set0 = new IntSingletonSet(ip0.asInstanceOf[IntValuePredicate].getTarget)
      } else {
        return false
      }
    }
    var set1: IntSet = null
    if (op1.isInstanceOf[Operation.OpAtom]) {
      set1 = getInitialChars(op1.asInstanceOf[Operation.OpAtom], caseBlind)
    } else if (op1.isInstanceOf[Operation.OpCharClass]) {
      val ip1 = op1.asInstanceOf[Operation.OpCharClass].predicate
      if (ip1.isInstanceOf[IntSetPredicate]) {
        set1 = ip1.asInstanceOf[IntSetPredicate].getIntSet
      } else if (ip1.isInstanceOf[IntValuePredicate]) {
        set1 = new IntSingletonSet(ip1.asInstanceOf[IntValuePredicate].getTarget)
      } else {
        return false
      }
    } else {
      return false
    }
    isDisjoint(set0, set1)
  }

  private def getInitialChars(op: Operation.OpAtom, caseBlind: Boolean): IntSet = {
    var set: IntSet = null
    val ch = op.atom.charAt(0)
    set = new IntSingletonSet(ch)
    if (caseBlind) {
      set = new IntHashSet(10)
      set.add(ch)
      for (v ← CaseVariants.getCaseVariants(ch)) {
        set.add(v)
      }
    }
    set
  }

  def isDisjoint(set0: IntSet, set1: IntSet): Boolean = {
    try {
      val intersection = set0.intersect(set1)
      intersection.isEmpty
    } catch {
      case e: Throwable ⇒ false
    }
  }
}

package org.orbeon.darius.xpath.regex

import scala.util.matching.Regex

class MatchResult(regexMatch: Regex.Match) {
  def getGroup(i: Int): String = {
    regexMatch.group(i)
  }
}

class RegExp(regex: Regex) {

  def exec(input: String): MatchResult = {
    regex.findFirstMatchIn(input) match {
      case Some(regexMatch) ⇒ new MatchResult(regexMatch)
      case None             ⇒ null
    }
  }

  def test(input: String): Boolean = {
    regex.findFirstMatchIn(input).isDefined
  }
}

object RegExp {
  def compile(pattern: CharSequence) = new RegExp(pattern.toString.r)
}

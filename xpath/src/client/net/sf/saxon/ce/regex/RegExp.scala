package client.net.sf.saxon.ce.regex

import scala.util.matching.Regex

class MatchResult {
  def getGroup(i: Int): String = ???
}

class RegExp(regex: Regex) {
  def exec(input: String): MatchResult = ???
  def test(input: String): Boolean = ???
}

object RegExp {
  def compile(pattern: CharSequence) = new RegExp(pattern.toString.r)
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import java.util.List

object ARegularExpression {

  def make(pattern: String): ARegularExpression = {
    new ARegularExpression(pattern, "", "XP20", null)
  }
}

/**
 * Glue class to interface the Jakarta regex engine to Saxon
 */
class ARegularExpression(
  pattern: CharSequence,
  var rawFlags: String,
  hostLanguage: String,
  warnings: List[String]
) extends RegularExpression {

  var rawPattern: UnicodeString = _

  var regex: REProgram = _

  var reFlags: REFlags = null

  reFlags = new REFlags(flags, hostLanguage)

  {
    rawPattern = GeneralUnicodeString.makeUnicodeString(pattern)
    val comp2 = new RECompiler()
    comp2.setFlags(reFlags)
    regex = comp2.compile(rawPattern)
    if (warnings != null) {
      for (s ← comp2.getWarnings) {
        warnings.add(s)
      }
    }
  }

  /**
   * Determine whether the regular expression matches a given string in its entirety
   *
   * @param input the string to match
   * @return true if the string matches, false otherwise
   */
  def matches(input: CharSequence): Boolean = {
    if (input.length == 0) {
      return regex.isNullable
    }
    val matcher = new REMatcher(regex)
    matcher.anchoredMatch(GeneralUnicodeString.makeUnicodeString(input))
  }

  /**
   * Determine whether the regular expression contains a match of a given string
   *
   * @param input the string to match
   * @return true if the string matches, false otherwise
   */
  def containsMatch(input: CharSequence): Boolean = {
    val matcher = new REMatcher(regex)
    matcher.`match`(GeneralUnicodeString.makeUnicodeString(input), 0)
  }

  /**
   * Use this regular expression to tokenize an input string.
   *
   * @param input the string to be tokenized
   * @return a SequenceIterator containing the resulting tokens, as objects of type StringValue
   */
  def tokenize(input: CharSequence): SequenceIterator = {
    new ATokenIterator(GeneralUnicodeString.makeUnicodeString(input), new REMatcher(regex))
  }

  /**
   * Use this regular expression to analyze an input string, in support of the XSLT
   * analyze-string instruction. The resulting RegexIterator provides both the matching and
   * non-matching substrings, and allows them to be distinguished. It also provides access
   * to matched subgroups.
   *
   * @param input the character string to be analyzed using the regular expression
   * @return an iterator over matched and unmatched substrings
   */
  def analyze(input: CharSequence): ARegexIterator = {
    new ARegexIterator(GeneralUnicodeString.makeUnicodeString(input), rawPattern, new REMatcher(regex))
  }

  /**
   * Replace all substrings of a supplied input string that match the regular expression
   * with a replacement string.
   *
   * @param input       the input string on which replacements are to be performed
   * @param replacement the replacement string in the format of the XPath replace() function
   * @return the result of performing the replacement
   * @throws XPathException if the replacement string is invalid
   */
  def replace(input: CharSequence, replacement: CharSequence): CharSequence = {
    val matcher = new REMatcher(regex)
    if (matcher.`match`("")) {
      throw new XPathException("The regular expression must not be one that matches a zero-length string", 
        "FORX0003")
    }
    val in = GeneralUnicodeString.makeUnicodeString(input)
    val rep = GeneralUnicodeString.makeUnicodeString(replacement)
    matcher.subst(in, rep)
  }
}

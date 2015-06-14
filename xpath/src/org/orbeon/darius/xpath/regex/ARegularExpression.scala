// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.orbeon.List

class ARegularExpression(
  pattern      : CharSequence,
  rawFlags     : String,
  hostLanguage : String,
  warnings     : List[String]
) extends RegularExpression {

  private val regex = pattern.toString.r

  // Used by the matches() function and DurationValue
  def containsMatch(input: CharSequence): Boolean =
    regex.findFirstMatchIn(input).isDefined

  // Used by replace(), tokenize(), and DurationValue
  def matches(input: CharSequence): Boolean =
    regex.pattern.matcher(input).matches

  // Used only by the replace() function
  def replace(input: CharSequence, replacement: CharSequence): CharSequence = ???

  // Used only by the tokenize() function
  def tokenize(input: CharSequence): SequenceIterator = ???
}

object ARegularExpression {
  def make(pattern: String): ARegularExpression =
    new ARegularExpression(pattern, "", "XP20", null)
}
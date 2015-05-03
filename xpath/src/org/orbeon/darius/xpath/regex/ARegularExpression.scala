// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.orbeon.List

//ORBEON placeholder
class ARegularExpression(
  pattern      : CharSequence,
  rawFlags     : String,
  hostLanguage : String,
  warnings     : List[String]
) extends RegularExpression {

//  private val r = pattern.toString.r

  def containsMatch(input: CharSequence): Boolean = ???
  def matches(input: CharSequence): Boolean = ???
  def replace(input: CharSequence, replacement: CharSequence): CharSequence = ???
  def tokenize(input: CharSequence): SequenceIterator = ???
}

object ARegularExpression {
  def make(pattern: String): ARegularExpression = new ARegularExpression(pattern, "", "XP20", null)
}
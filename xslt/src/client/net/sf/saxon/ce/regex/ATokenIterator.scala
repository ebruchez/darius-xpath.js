// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.value.StringValue

/**
 * A ATokenIterator is an iterator over the strings that result from tokenizing a string using a regular expression
 */
class ATokenIterator(var input: UnicodeString, var matcher: REMatcher) extends SequenceIterator {

  private var _current: UnicodeString = _

  private var prevEnd: Int = 0

  def next(): Item = {
    if (prevEnd < 0) {
      _current = null
      return null
    }
    if (matcher.`match`(input, prevEnd)) {
      val start = matcher.getParenStart(0)
      _current = input.substring(prevEnd, start)
      prevEnd = matcher.getParenEnd(0)
    } else {
      _current = input.substring(prevEnd, input.length)
      prevEnd = -1
    }
    currentStringValue()
  }

  private def currentStringValue(): Item = {
    if (_current.isInstanceOf[BMPString]) {
      StringValue.makeStringValue(_current.asInstanceOf[BMPString].getCharSequence)
    } else {
      StringValue.makeStringValue(_current.toString)
    }
  }

  def current(): Item = {
    if (_current == null) null else currentStringValue()
  }

  def close(): Unit = {
  }

  def getAnother: ATokenIterator = {
    new ATokenIterator(input, new REMatcher(matcher.getProgram))
  }
}

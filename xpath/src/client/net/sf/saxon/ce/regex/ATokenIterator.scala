package client.net.sf.saxon.ce.regex

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A ATokenIterator is an iterator over the strings that result from tokenizing a string using a regular expression
 */
class ATokenIterator(var input: UnicodeString, var matcher: REMatcher) extends SequenceIterator {

  private var current: UnicodeString = _

  private var prevEnd: Int = 0

  def next(): Item = {
    if (prevEnd < 0) {
      current = null
      return null
    }
    if (matcher.`match`(input, prevEnd)) {
      val start = matcher.getParenStart(0)
      current = input.substring(prevEnd, start)
      prevEnd = matcher.getParenEnd(0)
    } else {
      current = input.substring(prevEnd, input.length)
      prevEnd = -1
    }
    currentStringValue()
  }

  private def currentStringValue(): Item = {
    if (current.isInstanceOf[BMPString]) {
      StringValue.makeStringValue(current.asInstanceOf[BMPString].getCharSequence)
    } else {
      StringValue.makeStringValue(current.toString)
    }
  }

  def current(): Item = {
    (if (current == null) null else currentStringValue())
  }

  def close() {
  }

  def getAnother(): ATokenIterator = {
    new ATokenIterator(input, new REMatcher(matcher.getProgram))
  }
}

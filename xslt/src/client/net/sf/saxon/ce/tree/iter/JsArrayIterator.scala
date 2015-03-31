// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.tree.iter

import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.orbeon.Configuration
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.core.client.JsArray
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Class JsArrayIterator, iterates over a sequence of items held in a Javascript array; it also implements Sequence
 * providing access to the underlying value.
 */
class JsArrayIterator(var list: JsArray, var config: Configuration) extends UnfailingIterator with GroundedIterator with Sequence {

  var index: Int = 0

  var length: Int = list.length

  var current: Item = null

  def next(): Item = {
    if (index >= length) {
      current = null
      index = -1
      length = -1
      return null
    }
    val obj = getObject(index += 1, list)
    current = IXSLFunction.convertFromJavaScript(obj, config).next()
    current
  }

  private /* native */ def getObject(index: Int, jsa: JsArray): AnyRef

  /* native */ def getUnderlyingArray(): JavaScriptObject

  def current(): Item = current

  def getLastPosition: Int = length

  def getAnother: UnfailingIterator = new JsArrayIterator(list, config)

  /**
   * Return an iterator over this sequence.
   *
   * @return the required SequenceIterator, positioned at the start of the
   *     sequence
   */
  def iterate(): UnfailingIterator = getAnother

  /**
   * Return a Value containing all the items in the sequence returned by this
   * SequenceIterator. This should involve no computation, and throws no errors.
   *
   * @return the corresponding Value, or null if the value is not known
   */
  def materialize(): Sequence = this

  /**
   * Get the n'th item in the sequence (starting from 0).
   *
   * @param n position of the required item, counting from zero.
   * @return the n'th item in the sequence, where the first item in the sequence is
   *         numbered zero. If n is negative or >= the length of the sequence, returns null.
   */
  def itemAt(n: Int): Item = {
    IXSLFunction.convertFromJavaScript(getObject(n, list), config)
      .next()
  }

  /**
   * Get the length of the sequence
   *
   * @return the number of items in the sequence
   */
  def getLength: Int = length
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.js

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.StringValue
import com.google.gwt.core.client.JavaScriptObject
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An XPath item that encapsulates a JavaScript object. Such a value can only be obtained by
 * calling an extension function that returns it.
 */
class JSObjectValue(var jsObject: JavaScriptObject) extends Item {

  def getJavaScriptObject(): JavaScriptObject = jsObject

  def getStringValue: String = jsObject.toString

  def getTypedValue: AtomicValue = new StringValue(getStringValue)

  /**
   * Iterate over the items contained in this value.
   *
   * @return an iterator over the sequence of items
   */
  def iterate(): UnfailingIterator = SingletonIterator.makeIterator(this)

  /**
   * Get the n'th item in the sequence (starting from 0). This is defined for all
   * Values, but its real benefits come for a sequence Value stored extensionally
   * (or for a MemoClosure, once all the values have been read)
   *
   * @param n position of the required item, counting from zero.
   * @return the n'th item in the sequence, where the first item in the sequence is
   *         numbered zero. If n is negative or >= the length of the sequence, returns null.
   */
  def itemAt(n: Int): Item = if (n == 0) this else null

  /**
   * Get the length of the sequence
   *
   * @return the number of items in the sequence
   */
  def getLength: Int = 1
}

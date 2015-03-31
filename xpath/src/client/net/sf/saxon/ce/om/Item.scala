// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.value.AtomicValue

/**
 * An Item is an object that can occur as a member of a sequence.
 * It corresponds directly to the concept of an item in the XPath 2.0 data model.
 * There are two kinds of Item: atomic values, and nodes.
 * <p>
 * This interface is part of the public Saxon API. As such (starting from Saxon 8.4),
 * methods that form part of the stable API are labelled with a JavaDoc "since" tag
 * to identify the Saxon release at which they were introduced.
 * <p>
 * Note: there is no method getItemType(). This is to avoid having to implement it
 * on every implementation of NodeInfo. Instead, use the static method Type.getItemType(Item).
 *
 * @author Michael H. Kay
 * @since 8.4
 */
trait Item extends Sequence {

  /**
   * Get the value of the item as a string. For nodes, this is the string value of the
   * node as defined in the XPath 2.0 data model, except that all nodes are treated as being
   * untyped: it is not an error to get the string value of a node with a complex type.
   * For atomic values, the method returns the result of casting the atomic value to a string.
   *
   * @return the string value of the item
   * @throws UnsupportedOperationException if the item is a function item (an unchecked exception
   * is used here to avoid introducing exception handling to a large number of paths where it is not
   * needed)
   * @since 8.4
   */
  def getStringValue(): String

  /**
   * Get the typed value of the item.
   * <p>
   * If the node has not been validated against a schema, the typed value
   * will be the same as the string value, either as an instance of xs:string or as an instance
   * of xs:untypedAtomic, depending on the node kind.
   * <p>
   * For an atomic value, this method returns the atomic value itself.
   *
   * @return an iterator over the items in the typed value of the node or atomic value. The
   * items returned by this iterator will always be atomic values.
   
   * @since 8.4
   */
  def getTypedValue(): AtomicValue
}

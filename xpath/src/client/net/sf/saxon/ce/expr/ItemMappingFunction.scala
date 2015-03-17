package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ItemMappingFunction is an interface that must be satisfied by an object passed to a
 * ItemMappingIterator. It represents an object which, given an Item, can return either
 * another Item, or null.
 */
trait ItemMappingFunction {

  /**
   * Map one item to another item.
   * @param item The input item to be mapped.
   * @return either the output item, or null.
   */
  def mapItem(item: Item): Item
}

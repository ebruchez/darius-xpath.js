package client.net.sf.saxon.ce.`type`

import client.net.sf.saxon.ce.om.Item
import AnyItemType._
//remove if not needed
import scala.collection.JavaConversions._

object AnyItemType {

  private var theInstance: AnyItemType = new AnyItemType()

  /**
   * Factory method to get the singleton instance
   */
  def getInstance(): AnyItemType = theInstance
}

/**
 * An implementation of ItemType that matches any item (node or atomic value)
 */
class AnyItemType private () extends ItemType {

  /**
   * Test whether a given item conforms to this type
   *
   *
   * @param item The item to be tested
   * @return true if the item is an instance of this type; false otherwise
   */
  def matchesItem(item: Item): Boolean = true

  def getSuperType(): ItemType = null

  def getAtomizedItemType(): AtomicType = AtomicType.ANY_ATOMIC

  override def toString(): String = "item()"

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = "AnyItemType".hashCode
}

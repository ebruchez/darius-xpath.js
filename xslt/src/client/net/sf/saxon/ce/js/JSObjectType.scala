package client.net.sf.saxon.ce.js

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.`type`._
import JSObjectType._
//remove if not needed
import scala.collection.JavaConversions._

object JSObjectType {

  private var THE_INSTANCE: JSObjectType = new JSObjectType()

  def getInstance(): JSObjectType = THE_INSTANCE
}

/**
 *
 */
class JSObjectType extends ItemType {

  def matchesItem(item: Item): Boolean = item.isInstanceOf[JSObjectValue]

  def getSuperType(): ItemType = AnyItemType.getInstance

  def getAtomizedItemType(): AtomicType = null
}

package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.js.JSObjectValue
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.`type`._
import AnyJSObjectNodeTest._
//remove if not needed
import scala.collection.JavaConversions._

object AnyJSObjectNodeTest {

  private var THE_INSTANCE: AnyJSObjectNodeTest = new AnyJSObjectNodeTest()

  def getInstance(): AnyJSObjectNodeTest = THE_INSTANCE
}

class AnyJSObjectNodeTest extends NodeTest {

  def matchesItem(item: Item): Boolean = item.isInstanceOf[JSObjectValue]

  def getSuperType(): ItemType = AnyItemType.getInstance

  def getRequiredNodeKind(): Int = Type.ITEM

  def getAtomizedItemType(): AtomicType = null

  override def getDefaultPriority(): Double = 0

  override def matches(nodeKind: Int, qName: StructuredQName): Boolean = false
}

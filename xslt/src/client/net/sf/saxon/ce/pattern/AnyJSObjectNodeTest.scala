// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.pattern

import org.orbeon.darius.xpath.js.JSObjectValue
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.`type`._
import AnyJSObjectNodeTest._

import scala.collection.JavaConversions._

object AnyJSObjectNodeTest {

  private var THE_INSTANCE: AnyJSObjectNodeTest = new AnyJSObjectNodeTest()

  def getInstance(): AnyJSObjectNodeTest = THE_INSTANCE
}

class AnyJSObjectNodeTest extends NodeTest {

  def matchesItem(item: Item): Boolean = item.isInstanceOf[JSObjectValue]

  def getSuperType: ItemType = AnyItemType.getInstance

  def getRequiredNodeKind: Int = Type.ITEM

  def getAtomizedItemType: AtomicType = null

  override def getDefaultPriority: Double = 0

  override def matches(nodeKind: Int, qName: StructuredQName): Boolean = false
}

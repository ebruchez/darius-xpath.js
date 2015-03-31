// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
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

  def getSuperType: ItemType = AnyItemType.getInstance

  def getAtomizedItemType: AtomicType = null
}

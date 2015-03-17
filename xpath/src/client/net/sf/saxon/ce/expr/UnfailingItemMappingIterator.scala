package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
//remove if not needed
import scala.collection.JavaConversions._

class UnfailingItemMappingIterator(base: UnfailingIterator, action: ItemMappingFunction)
    extends ItemMappingIterator(base, action) with UnfailingIterator {

  override def next(): Item = super.next()

  protected override def getBaseIterator(): UnfailingIterator = {
    super.getBaseIterator.asInstanceOf[UnfailingIterator]
  }

  override def getAnother(): UnfailingIterator = {
    val newBase = getBaseIterator.getAnother
    val action = getMappingFunction
    val newAction = if (action.isInstanceOf[StatefulMappingFunction]) action.asInstanceOf[StatefulMappingFunction].getAnother(newBase).asInstanceOf[ItemMappingFunction] else action
    new UnfailingItemMappingIterator(newBase, newAction)
  }
}

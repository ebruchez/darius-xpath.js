package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.ItemMappingFunction
import client.net.sf.saxon.ce.expr.ItemMappingIterator
import client.net.sf.saxon.ce.expr.StatefulMappingFunction
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import java.util.HashSet
import DistinctValues._
//remove if not needed
import scala.collection.JavaConversions._

object DistinctValues {

  class DistinctItemsMappingFunction(var collator: StringCollator, var implicitTimezone: Int)
      extends ItemMappingFunction with StatefulMappingFunction {

    private var lookup: HashSet[Any] = new HashSet[Any](40)

    def mapItem(item: Item): Item = {
      val value = item.asInstanceOf[AtomicValue]
      var key: AnyRef = null
      key = if (value.isNaN) classOf[DistinctValues] else value.getXPathComparable(false, collator, implicitTimezone)
      if (lookup.add(key)) {
        item
      } else {
        null
      }
    }

    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = {
      new DistinctItemsMappingFunction(collator, implicitTimezone)
    }
  }
}

/**
 * The XPath 2.0 distinct-values() function
 */
class DistinctValues extends CollatingFunction {

  def newInstance(): DistinctValues = new DistinctValues()

  /**
   * Evaluate the function to return an iteration of selected values or nodes.
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val collator = getCollator(1, context)
    val iter = argument(0).iterate(context)
    val function = new DistinctItemsMappingFunction(collator, context.getImplicitTimezone)
    new ItemMappingIterator(iter, function)
  }
}

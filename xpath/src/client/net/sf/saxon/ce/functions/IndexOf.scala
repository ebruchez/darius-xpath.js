package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.ItemMappingFunction
import client.net.sf.saxon.ce.expr.ItemMappingIterator
import client.net.sf.saxon.ce.expr.StatefulMappingFunction
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.AtomicComparer
import client.net.sf.saxon.ce.expr.sort.GenericAtomicComparer
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.IntegerValue
import IndexOf._
//remove if not needed
import scala.collection.JavaConversions._

object IndexOf {

  class IndexOfMappingFunction(var searchType: AtomicType, var comparer: AtomicComparer, var `val`: AtomicValue)
      extends ItemMappingFunction with StatefulMappingFunction {

    var index: Int = 0

    def mapItem(item: Item): IntegerValue = {
      index += 1
      if (Type.isComparable(searchType, item.asInstanceOf[AtomicValue].getItemType, false) && 
        comparer.comparesEqual(item.asInstanceOf[AtomicValue], `val`)) {
        new IntegerValue(index)
      } else {
        null
      }
    }

    /**
     * Return a clone of this MappingFunction, with the state reset to its state at the beginning
     * of the underlying iteration
     *
     * @return a clone of this MappingFunction
     * @param newBaseIterator
     */
    def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction = {
      new IndexOfMappingFunction(searchType, comparer, `val`)
    }
  }
}

/**
 * The XPath 2.0 index-of() function
 */
class IndexOf extends CollatingFunction {

  def newInstance(): IndexOf = new IndexOf()

  /**
   * Evaluate the function to return an iteration of selected items.
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val comparer = getAtomicComparer(2, context)
    val seq = argument(0).iterate(context)
    val `val` = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
    val searchType = `val`.getItemType
    new ItemMappingIterator(seq, new IndexOfMappingFunction(searchType, comparer, `val`))
  }
}

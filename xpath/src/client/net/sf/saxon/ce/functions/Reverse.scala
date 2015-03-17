package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.SequenceExtent
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implement XPath function fn:reverse()
 */
class Reverse extends SystemFunction {

  def newInstance(): Reverse = new Reverse()

  /**
   * Determine the item type of the value returned by the function
   *
   */
  def getItemType(): ItemType = argument(0).getItemType

  def computeSpecialProperties(): Int = {
    val baseProps = argument(0).getSpecialProperties
    if ((baseProps & StaticProperty.REVERSE_DOCUMENT_ORDER) != 
      0) {
      (baseProps & (~StaticProperty.REVERSE_DOCUMENT_ORDER)) | 
        StaticProperty.ORDERED_NODESET
    } else if ((baseProps & StaticProperty.ORDERED_NODESET) != 0) {
      (baseProps & (~StaticProperty.ORDERED_NODESET)) | StaticProperty.REVERSE_DOCUMENT_ORDER
    } else {
      baseProps
    }
  }

  def iterate(context: XPathContext): SequenceIterator = {
    val forwards = argument(0).iterate(context)
    val extent = SequenceExtent.makeReversed(forwards)
    extent.iterate()
  }
}

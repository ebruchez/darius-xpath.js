package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.GroupIterator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implements the XSLT function current-grouping-key()
 */
class CurrentGroupingKey extends SystemFunction {

  def newInstance(): CurrentGroupingKey = new CurrentGroupingKey()

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CURRENT_GROUP

  /**
   * Evaluate the expression
   */
  def evaluateItem(c: XPathContext): Item = {
    val gi = c.getCurrentGroupIterator
    if (gi == null) {
      return null
    }
    gi.getCurrentGroupingKey
  }
}

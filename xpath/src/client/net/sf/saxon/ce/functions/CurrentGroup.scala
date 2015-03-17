package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.GroupIterator
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implements the XSLT function current-group()
 */
class CurrentGroup extends SystemFunction {

  def newInstance(): CurrentGroup = new CurrentGroup()

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CURRENT_GROUP

  /**
   * Return an iteration over the result sequence
   */
  def iterate(c: XPathContext): SequenceIterator = {
    val gi = c.getCurrentGroupIterator
    if (gi == null) {
      return EmptyIterator.getInstance
    }
    gi.iterateCurrentGroup()
  }
}

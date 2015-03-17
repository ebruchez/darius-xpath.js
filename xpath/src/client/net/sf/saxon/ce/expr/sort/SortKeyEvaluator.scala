package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Callback interface used to evaluate sort keys. An instance of this class is passed to the
 * SortedIterator, and is used whenever a sort key value needs to be computed.
 */
trait SortKeyEvaluator {

  /**
   * Evaluate the n'th sort key of the context item
   */
  def evaluateSortKey(n: Int, context: XPathContext): Item
}

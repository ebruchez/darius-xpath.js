package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.NodeInfo
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Comparer used for comparing nodes in document order
 *
 * @author Michael H. Kay
 *
 */
trait NodeOrderComparer {

  /**
   * Compare two objects.
   * @return <0 if a<b, 0 if a=b, >0 if a>b
   */
  def compare(a: NodeInfo, b: NodeInfo): Int
}

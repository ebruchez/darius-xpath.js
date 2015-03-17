package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.NodeInfo
import GlobalOrderComparer._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object GlobalOrderComparer {

  @BeanProperty
  var instance: GlobalOrderComparer = new GlobalOrderComparer()
}

/**
 * A Comparer used for comparing nodes in document order. This
 * comparer is used when there is no guarantee that the nodes being compared
 * come from the same document
 *
 * @author Michael H. Kay
 *
 */
class GlobalOrderComparer extends NodeOrderComparer {

  def compare(a: NodeInfo, b: NodeInfo): Int = {
    if (a == b) {
      return 0
    }
    val d1 = a.getDocumentNumber
    val d2 = b.getDocumentNumber
    if (d1 == d2) {
      return a.compareOrder(b)
    }
    Long.signum(d1 - d2)
  }
}

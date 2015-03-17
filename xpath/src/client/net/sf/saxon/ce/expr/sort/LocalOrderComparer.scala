package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.om.NodeInfo
import LocalOrderComparer._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object LocalOrderComparer {

  @BeanProperty
  var instance: LocalOrderComparer = new LocalOrderComparer()
}

/**
 * A Comparer used for comparing nodes in document order. This
 * comparer assumes that the nodes being compared come from the same document
 *
 * @author Michael H. Kay
 *
 */
class LocalOrderComparer extends NodeOrderComparer {

  def compare(a: NodeInfo, b: NodeInfo): Int = {
    val n1 = a.asInstanceOf[NodeInfo]
    val n2 = b.asInstanceOf[NodeInfo]
    n1.compareOrder(n2)
  }
}

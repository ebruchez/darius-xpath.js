package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.GroundedIterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A LastItemExpression returns the last item in the sequence returned by a given
 * base expression. The evaluation strategy is to read the input sequence with a one-item lookahead.
 */
class LastItemExpression(base: Expression) extends SingleItemFilter {

  operand = base

  adoptChildExpression(base)

  computeStaticProperties()

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val forwards = operand.iterate(context)
    if (forwards.isInstanceOf[GroundedIterator]) {
      val `val` = forwards.asInstanceOf[GroundedIterator].materialize()
      if (`val` != null) {
        return `val`.itemAt(`val`.getLength - 1)
      }
    }
    var current: Item = null
    while (true) {
      val item = forwards.next()
      if (item == null) {
        return current
      }
      current = item
    }
  }
}

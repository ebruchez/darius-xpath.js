package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A FirstItemExpression returns the first item in the sequence returned by a given
 * base expression
 */
class FirstItemExpression(base: Expression) extends SingleItemFilter {

  operand = base

  adoptChildExpression(base)

  computeStaticProperties()

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val iter = operand.iterate(context)
    iter.next()
  }
}

package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.Cardinality
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * xf:string-join(string* $sequence, string $separator)
 */
class StringJoin extends SystemFunction {

  def newInstance(): StringJoin = new StringJoin()

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val exp = super.optimize(visitor, contextItemType)
    if (exp.isInstanceOf[StringJoin]) {
      exp.asInstanceOf[StringJoin].simplifySingleton()
    } else {
      exp
    }
  }

  private def simplifySingleton(): Expression = {
    val card = argument(0).getCardinality
    if (!Cardinality.allowsMany(card)) {
      if (Cardinality.allowsZero(card)) {
        return SystemFunction.makeSystemFunction("string", Array(argument(0)))
      } else {
        return argument(0)
      }
    }
    this
  }

  def evaluateItem(c: XPathContext): Item = {
    val iter = argument(0).iterate(c)
    var it = iter.next()
    if (it == null) {
      return StringValue.EMPTY_STRING
    }
    val first = it.getStringValue
    it = iter.next()
    if (it == null) {
      return StringValue.makeStringValue(first)
    }
    val sb = new FastStringBuffer(FastStringBuffer.SMALL)
    sb.append(first)
    val sep = argument(1).evaluateItem(c).getStringValue
    sb.append(sep)
    sb.append(it.getStringValue)
    while (true) {
      it = iter.next()
      if (it == null) {
        return StringValue.makeStringValue(sb.condense())
      }
      sb.append(sep)
      sb.append(it.getStringValue)
    }
  }
}

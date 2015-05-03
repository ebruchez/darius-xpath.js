// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr.{Expression, ExpressionVisitor, XPathContext}
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.tree.util.FastStringBuffer
import org.orbeon.darius.xpath.value.{Cardinality, StringValue}

/**
 * xf:string-join(string* $sequence, string $separator)
 */
class StringJoin extends SystemFunction {

  def newInstance(): StringJoin = new StringJoin()

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val exp = super.optimize(visitor, contextItemType)
    exp match {
      case join: StringJoin ⇒
        join.simplifySingleton()
      case _ ⇒
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

  override def evaluateItem(c: XPathContext): Item = {
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
    throw new IllegalStateException
  }
}

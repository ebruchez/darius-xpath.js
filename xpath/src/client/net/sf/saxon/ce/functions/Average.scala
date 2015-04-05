// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value._

/**
 * Implementation of the fn:avg function
 */
class Average extends Aggregate {

  def newInstance(): Average = new Average()

  /**
   * Determine the item type of the value returned by the function
   */
  override def getItemType: ItemType = {
    val base = Atomizer.getAtomizedItemType(argument(0), alwaysUntyped = false)
    if (base == AtomicType.UNTYPED_ATOMIC) {
      AtomicType.DOUBLE
    } else if (base == AtomicType.INTEGER) {
      AtomicType.DECIMAL
    } else {
      base
    }
  }

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val iter = argument(0).iterate(context)
    var count = 0
    var item = iter.next().asInstanceOf[AtomicValue]
    if (item == null) {
      return null
    }
    count += 1
    if (item.isInstanceOf[UntypedAtomicValue]) {
      try {
        item = item.convert(AtomicType.DOUBLE).asAtomic()
      } catch {
        case e: XPathException ⇒
          e.maybeSetLocation(getSourceLocator)
          throw e
      }
    }
    item match {
      case _: NumericValue ⇒
        while (true) {
          var next = iter.next().asInstanceOf[AtomicValue]
          if (next == null) {
            return ArithmeticExpression.compute(item, Token.DIV, new IntegerValue(count), context)
          }
          count += 1
          if (next.isInstanceOf[UntypedAtomicValue]) {
            next = next.convert(AtomicType.DOUBLE).asAtomic()
          } else if (!next.isInstanceOf[NumericValue]) {
            badMix(context)
          }
          item = ArithmeticExpression.compute(item, Token.PLUS, next, context)
        }
        throw new IllegalStateException
      case value: DurationValue ⇒
        while (true) {
          val next = iter.next().asInstanceOf[AtomicValue]
          if (next == null) {
            return value.multiply(1.0 / count)
          }
          count += 1
          if (!next.isInstanceOf[DurationValue]) {
            badMix(context)
          }
          item = value.add(next.asInstanceOf[DurationValue])
        }
        throw new IllegalStateException
      case _ ⇒
        badMix(context)
        null
    }
  }

  private def badMix(context: XPathContext): Unit = {
    dynamicError("Input to avg() contains invalid or mixed data types", "FORG0006")
  }
}

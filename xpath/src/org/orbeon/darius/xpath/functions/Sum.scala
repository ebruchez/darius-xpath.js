// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.{AtomicType, ItemType, Type}
import org.orbeon.darius.xpath.expr.{ArithmeticExpression, Atomizer, Token, XPathContext}
import org.orbeon.darius.xpath.functions.Sum._
import org.orbeon.darius.xpath.om.{Item, SequenceIterator}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.SourceLocator
import org.orbeon.darius.xpath.value._

object Sum {

  /**
   * Calculate the total of a sequence.
   * @param iter iterator over the items to be totalled
   * @param context the XPath dynamic context
   * @param location location of the expression in the source for diagnostics
   * @return the total, according to the rules of the XPath sum() function, but returning null
   * if the sequence is empty. (It's then up to the caller to decide what the correct result is
   * for an empty sequence.
   */
  def total(iter: SequenceIterator, context: XPathContext, location: SourceLocator): AtomicValue = {
    var sum = iter.next().asInstanceOf[AtomicValue]
    if (sum == null) {
      return null
    }
    if (sum.isInstanceOf[UntypedAtomicValue]) {
      try {
        sum = sum.convert(AtomicType.DOUBLE).asAtomic()
      } catch {
        case e: XPathException ⇒
          e.maybeSetLocation(location)
          throw e
      }
    }
    sum match {
      case _: NumericValue ⇒
        while (true) {
          var next = iter.next().asInstanceOf[AtomicValue]
          if (next == null) {
            return sum
          }
          if (next.isInstanceOf[UntypedAtomicValue]) {
            next = next.convert(AtomicType.DOUBLE).asAtomic()
          } else if (!next.isInstanceOf[NumericValue]) {
            throw new XPathException("Input to sum() contains a mix of numeric and non-numeric values",
              "FORG0006", location)
          }
          sum = ArithmeticExpression.compute(sum, Token.PLUS, next, context)
          if (sum.isNaN && sum.isInstanceOf[DoubleValue]) {
            return sum
          }
        }
        throw new IllegalStateException
      case value: DurationValue ⇒
        if (!(sum.isInstanceOf[DayTimeDurationValue] || sum.isInstanceOf[YearMonthDurationValue])) {
          throw new XPathException("Input to sum() contains a duration that is neither a dayTimeDuration nor a yearMonthDuration",
            "FORG0006", location)
        }
        while (true) {
          val next = iter.next().asInstanceOf[AtomicValue]
          if (next == null) {
            return sum
          }
          if (!next.isInstanceOf[DurationValue]) {
            throw new XPathException("Input to sum() contains a mix of duration and non-duration values",
              "FORG0006", location)
          }
          sum = value.add(next.asInstanceOf[DurationValue])
        }
        throw new IllegalStateException
      case _ ⇒
        throw new XPathException("Input to sum() contains a value of type " + sum.getItemType.getDisplayName +
          " which is neither numeric, nor a duration", "FORG0006", location)
    }
  }
}

/**
 * Implementation of the fn:sum function
 */
class Sum extends Aggregate {

  def newInstance(): Sum = new Sum()

  override def getItemType: ItemType = {
    var base = Atomizer.getAtomizedItemType(argument(0), alwaysUntyped = false)
    if (base == AtomicType.UNTYPED_ATOMIC) {
      base = AtomicType.DOUBLE
    }
    if (Cardinality.allowsZero(argument(0).getCardinality)) {
      if (argument.length == 1) {
        Type.getCommonSuperType(base, AtomicType.INTEGER)
      } else {
        Type.getCommonSuperType(base, argument(1).getItemType)
      }
    } else {
      base
    }
  }

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val sum = total(argument(0).iterate(context), context, getSourceLocator)
    if (sum != null) {
      sum
    } else {
      if (argument.length == 2) {
        argument(1).evaluateItem(context)
      } else {
        IntegerValue.ZERO
      }
    }
  }
}

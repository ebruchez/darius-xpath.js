// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.functions.Rounding._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value.{AtomicValue, NumericValue}

object Rounding {

  val FLOOR = 0
  val CEILING = 1
  val ROUND = 2
  val HALF_EVEN = 3
  val ABS = 4
}

/**
 * This class supports the ceiling(), floor(), round(), and round-to-half-even() functions,
 * and also the abs() function
 */
class Rounding(_operation: Int) extends SystemFunction {

  this.operation = _operation

  def newInstance(): Rounding = new Rounding(operation)

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val val0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (val0 == null) {
      return null
    }
    val `val` = val0.asInstanceOf[NumericValue]
    operation match {
      case FLOOR ⇒ `val`.floor()
      case CEILING ⇒ `val`.ceiling()
      case ROUND ⇒ `val`.round()
      case HALF_EVEN ⇒
        var scale = 0
        if (argument.length == 2) {
          val scaleVal0 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
          val scaleVal = scaleVal0.asInstanceOf[NumericValue]
          scale = scaleVal.intValue().toInt
        }
        `val`.roundHalfToEven(scale)

      case ABS ⇒ `val`.abs()
      case _ ⇒ throw new UnsupportedOperationException("Unknown rounding function")
    }
  }

  /**
   * Determine the cardinality of the function.
   */
  override def computeCardinality(): Int = argument(0).getCardinality
}

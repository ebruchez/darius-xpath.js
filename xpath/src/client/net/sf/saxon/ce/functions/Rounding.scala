package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.NumericValue
import Rounding._
//remove if not needed
import scala.collection.JavaConversions._

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
class Rounding(operation: Int) extends SystemFunction {

  this.operation = operation

  def newInstance(): Rounding = new Rounding(operation)

  /**
   * Evaluate the function
   */
  def evaluateItem(context: XPathContext): Item = {
    val val0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (val0 == null) {
      return null
    }
    val `val` = val0.asInstanceOf[NumericValue]
    operation match {
      case FLOOR => `val`.floor()
      case CEILING => `val`.ceiling()
      case ROUND => `val`.round()
      case HALF_EVEN => 
        var scale = 0
        if (argument.length == 2) {
          val scaleVal0 = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
          val scaleVal = scaleVal0.asInstanceOf[NumericValue]
          scale = scaleVal.intValue().toInt
        }
        `val`.roundHalfToEven(scale)

      case ABS => `val`.abs()
      case _ => throw new UnsupportedOperationException("Unknown rounding function")
    }
  }

  /**
   * Determine the cardinality of the function.
   */
  def computeCardinality(): Int = argument(0).getCardinality
}

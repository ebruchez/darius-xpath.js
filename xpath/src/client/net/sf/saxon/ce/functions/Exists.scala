package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.FunctionCall
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.BooleanValue
import Exists._
//remove if not needed
import scala.collection.JavaConversions._

object Exists {

  val EXISTS = 2

  val EMPTY = 3
}

/**
 * Implementation of the fn:exists and fn:empty functions
 */
class Exists(operation: Int) extends Aggregate {

  this.operation = operation

  def newInstance(): Exists = new Exists(operation)

  /**
   * Return the negation of the expression
   * @return the negation of the expression
   */
  def negate(): Expression = {
    val fc = SystemFunction.makeSystemFunction((if (operation == EXISTS) "empty" else "exists"), getArguments)
    fc.setSourceLocator(getSourceLocator)
    fc
  }

  /**
   * Evaluate the function
   */
  def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Evaluate the function in a boolean context
   */
  def effectiveBooleanValue(c: XPathContext): Boolean = {
    val next = argument(0).iterate(c).next()
    (if (operation == EXISTS) next != null else next == null)
  }
}

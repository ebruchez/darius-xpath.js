// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.Trace._
import client.net.sf.saxon.ce.om.{Item, NodeInfo, Sequence, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.Logger
import client.net.sf.saxon.ce.trace.Location
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.value.SequenceExtent
import client.net.sf.saxon.ce.{LogConfiguration, LogController}

object Trace {

  private val logger: Logger = Logger.getLogger("Trace")

  def traceItem(`val`: Item, label: String): Unit = {
    if (`val` == null) {
      logger.info(label + ": empty sequence")
    } else {
      `val` match {
        case node: NodeInfo ⇒
          logger.info(label + ": " + Type.displayTypeName(`val`) + ": " + Navigator.getPath(node))
        case _ ⇒
          logger.info(label + ": " + Type.displayTypeName(`val`) + ": " + `val`.getStringValue)
      }
    }
  }
}

/**
 * This class supports the XPath 2.0 function trace().
 * The value is traced to the Logger output if logLevel is FINE, if logLevel is FINEST then
 * TraceListener is in use, in which case the information is sent to the TraceListener
 */
class Trace extends SystemFunction {

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-significant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = argument(0).getSpecialProperties

  /**
   * Get the static cardinality
   */
  override def computeCardinality(): Int = argument(0).getCardinality

  /**
   * Evaluate the function
   */
  override def evaluateItem(context: XPathContext): Item = {
    val `val` = argument(0).evaluateItem(context)
    if (LogConfiguration.loggingIsEnabled()) {
      val label = argument(1).evaluateAsString(context).toString
      if (LogController.traceIsEnabled()) {
        notifyListener(label, `val`, context)
      } else {
        traceItem(`val`, label)
      }
    }
    `val`
  }

  private def notifyListener(label: String, `val`: Sequence, context: XPathContext): Unit = {
    val info = new TraceExpression(this)
    info.setConstructType(Location.TRACE_CALL)
    info.setSourceLocator(this.getSourceLocator)
    info.setProperty("label", label)
    info.setProperty("value", `val`)
    val listener = LogController.getTraceListener
    listener.enter(info, context)
    listener.leave(info)
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      val label = argument(1).evaluateAsString(context).toString
      val evalMode = ExpressionTool.eagerEvaluationMode(argument(0))
      val value = ExpressionTool.evaluate(argument(0), evalMode, context)
      notifyListener(label, value, context)
      value.iterate()
    } else {
      if (!LogConfiguration.loggingIsEnabled()) {
        argument(0).iterate(context)
      } else {
        new TracingIterator(argument(0).iterate(context), argument(1).evaluateAsString(context).toString)
      }
    }
  }

  /**
   * Evaluate the expression
   *
   * @param arguments the values of the arguments, supplied as SequenceIterators
   * @param context   the dynamic evaluation context
   * @return the result of the evaluation, in the form of a SequenceIterator
   * @throws XPathException
   *          if a dynamic error occurs during the evaluation of the expression
   */
  def call(arguments: Array[SequenceIterator], context: XPathContext): SequenceIterator = {
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      val label = arguments(1).next().getStringValue
      val value = SequenceExtent.makeSequenceExtent(arguments(0))
      notifyListener(label, value, context)
      value.iterate()
    } else {
      if (!LogConfiguration.loggingIsEnabled()) {
        argument(0).iterate(context)
      } else {
        new TracingIterator(argument(0).iterate(context), argument(1).evaluateAsString(context).toString)
      }
    }
  }

  /**
   * Tracing Iterator class
   */
  private class TracingIterator(var base: SequenceIterator, var label: String)
      extends SequenceIterator {

    var empty: Boolean = true

    var pos: Int = 0

    def next(): Item = {
      val n = base.next()
      if (n == null) {
        if (empty) {
          traceItem(null, label)
        }
      } else {
        traceItem(n, label + " [" + pos + ']')
        empty = false
      }
      n
    }

    def getAnother: SequenceIterator = {
      new TracingIterator(base.getAnother, label)
    }
  }

  override def newInstance(): SystemFunction = new Trace()
}

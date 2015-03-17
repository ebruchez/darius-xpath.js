package client.net.sf.saxon.ce.trace

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.StaticContext
import client.net.sf.saxon.ce.om.StructuredQName
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A code injector that wraps every expression (other than a literal) in a TraceExpression, which causes
 * a TraceListener to be notified when the expression is evaluated
 */
class XSLTTraceCodeInjector extends TraceCodeInjector {

  /**
   * If tracing, wrap an expression in a trace instruction
   *
   *
   * @param exp         the expression to be wrapped
   * @param env         the static context
   * @param construct   integer constant identifying the kind of construct
   * @param qName       the name of the construct (if applicable)
   * @return the expression that does the tracing
   */
  def inject(exp: Expression, 
      env: StaticContext, 
      construct: StructuredQName, 
      qName: StructuredQName): Expression = {
    if (XSLTTraceListener.tagName(construct) != null) {
      super.inject(exp, env, construct, qName)
    } else {
      exp
    }
  }
}

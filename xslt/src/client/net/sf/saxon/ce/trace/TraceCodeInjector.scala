// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trace

import java.util.ArrayList
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.StaticContext
import client.net.sf.saxon.ce.expr.TraceExpression
import client.net.sf.saxon.ce.expr.parser.CodeInjector
import client.net.sf.saxon.ce.om.StructuredQName
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A code injector that wraps every expression (other than a literal) in a TraceExpression, which causes
 * a TraceListener to be notified when the expression is evaluated
 */
class TraceCodeInjector extends CodeInjector {

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
    if (exp.isInstanceOf[Literal]) {
      return exp
    }
    val trace = new TraceExpression(exp)
    trace.setNamespaceResolver(env.getNamespaceResolver)
    trace.setConstructType(construct)
    trace.setObjectName(qName)
    val properties = exp.getTraceProperties
    if (properties != null) {
      for (property <- properties) {
        trace.setProperty(property(0), property(1))
      }
    }
    trace
  }
}

package client.net.sf.saxon.ce.expr.parser

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.StaticContext
import client.net.sf.saxon.ce.om.StructuredQName
//remove if not needed
import scala.collection.JavaConversions._

/**
 *  A code injector can be used to add code to the expression tree (for example, diagnostic tracing code)
 *  during the process of parsing and tree construction
 */
trait CodeInjector {

  /**
   * If tracing, wrap an expression in a trace instruction
   *
   *
   * @param exp         the expression to be wrapped
   * @param env         the static context
   * @param construct   identifies the kind of construct
   * @param qName       the name of the construct (if applicable)
   * @return a replacement for the original expression (or the original expression unchanged)
   */
  def inject(exp: Expression, 
      env: StaticContext, 
      construct: StructuredQName, 
      qName: StructuredQName): Expression
}

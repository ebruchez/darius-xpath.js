package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for local xsl:variable elements in stylesheet. Not used in XQuery. In fact, the class is used
 * only transiently in XSLT: local variables are compiled first to a LocalVariable object, and subsequently
 * to a LetExpression.
 */
class LocalVariable extends GeneralVariable {

  /**
   * Process the local variable declaration
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    throw new UnsupportedOperationException("LocalVariable")
  }

  /**
   * Evaluate the variable
   */
  def evaluateVariable(c: XPathContext): Sequence = {
    throw new UnsupportedOperationException("LocalVariable")
  }
}

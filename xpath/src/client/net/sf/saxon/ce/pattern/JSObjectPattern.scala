package client.net.sf.saxon.ce.pattern

import com.google.gwt.core.client.JavaScriptObject
import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import JSObjectPattern._
//remove if not needed
import scala.collection.JavaConversions._

object JSObjectPattern {

  private /* native */ def testEquality(a: JavaScriptObject, b: JavaScriptObject): Boolean
}

class JSObjectPattern(var expression: Expression, config: Configuration) extends NodeSetPattern(exp) {

  private var `val`: JavaScriptObject = null

  def getNodeKind(): Int = Type.EMPTY

  /**
   * Get a NodeTest that all the nodes matching this pattern must satisfy
   */
  def getNodeTest(): NodeTest = AnyJSObjectNodeTest.getInstance

  /**
   * Evaluate the pattern - it should normally be ixsl:window() - uses
   * local variable to cache value so it can be used for a match test
   */
  def evaluate(context: XPathContext): JavaScriptObject = {
    val valueRep = expression.evaluateItem(context)
    `val` = IXSLFunction.convertToJavaScript(valueRep).asInstanceOf[JavaScriptObject]
    `val`
  }

  def matchesObject(obj: JavaScriptObject): Boolean = testEquality(`val`, obj)
}

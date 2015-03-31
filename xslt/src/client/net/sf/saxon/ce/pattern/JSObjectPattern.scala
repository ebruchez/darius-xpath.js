// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.orbeon.Configuration
import com.google.gwt.core.client.JavaScriptObject
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import JSObjectPattern._

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

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.{Expression, ExpressionVisitor, XPathContext}
import client.net.sf.saxon.ce.lib.ErrorListener
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{AtomicValue, BooleanValue}

object ErrorDiscarder {
  val THE_INSTANCE = new ErrorListener {
    def error(exception: XPathException) = ()
  }
}

/**
 * Implement the fn:doc-available() function
 */
class DocAvailable extends SystemFunction {

  def newInstance(): DocAvailable = new DocAvailable()

  private var expressionBaseURI: String = null

  override def checkArguments(visitor: ExpressionVisitor) {
    if (expressionBaseURI == null) {
      super.checkArguments(visitor)
      expressionBaseURI = visitor.getStaticContext.getBaseURI
    }
  }

  /**
   * Get the static base URI of the expression
   */
  def getStaticBaseURI(): String = expressionBaseURI

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Evaluate the expression
   * @param context
   * @return the result of evaluating the expression (a BooleanValue)
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  override def evaluateItem(context: XPathContext): Item = {
    val hrefVal = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (hrefVal == null) {
      return BooleanValue.FALSE
    }
    val href = hrefVal.getStringValue
    val controller = context.getController
    val old = controller.getErrorListener
    controller.setErrorListener(ErrorDiscarder.THE_INSTANCE)
    val b = docAvailable(href, context)
    controller.setErrorListener(old)
    BooleanValue.get(b)
  }

  private def docAvailable(href: String, context: XPathContext): Boolean = {
    ???
// ORBEON NIY
//    try {
//      val documentKey = DocumentFn.computeDocumentKey(href, expressionBaseURI)
//      val pool = context.getController.getDocumentPool
//      if (pool.isMarkedUnavailable(documentKey)) {
//        return false
//      }
//      val doc = pool.find(documentKey)
//      if (doc != null) {
//        return true
//      }
//      val item = DocumentFn.makeDoc(href, expressionBaseURI, context, getSourceLocator)
//      if (item != null) {
//        true
//      } else {
//        pool.markUnavailable(documentKey)
//        false
//      }
//    } catch {
//      case e: XPathException => false
//    }
  }
}

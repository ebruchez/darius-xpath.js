// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.{Expression, ExpressionVisitor, XPathContext}
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.QNameValue

/**
 * Implement XPath function fn:error()
 */
class Error extends SystemFunction {

  def newInstance(): Error = new Error()

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Evaluation of the expression always throws an error
   */
  override def evaluateItem(context: XPathContext): Item = {
    var qname: QNameValue = null
    if (argument.length > 0) {
      qname = argument(0).evaluateItem(context).asInstanceOf[QNameValue]
    }
    if (qname == null) {
      qname = new QNameValue("err", NamespaceConstant.ERR, (if (argument.length == 1) "FOTY0004" else "FOER0000"))
    }
    var description: String = null
    description = if (argument.length > 1) argument(1).evaluateItem(context).getStringValue else "Error signalled by application call on error()"
    val e = new XPathException(description)
    e.setErrorCodeQName(qname.toStructuredQName())
    e.setLocator(getSourceLocator)
    throw e
  }
}

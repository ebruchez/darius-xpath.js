// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.{Item, NameChecker, QNameException}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{AtomicValue, QNameValue}

/**
 * This class supports the fn:QName() function (previously named fn:expanded-QName())
 */
class QNameFn extends SystemFunction {

  def newInstance(): QNameFn = new QNameFn()

  /**
   * Pre-evaluate a function at compile time. Functions that do not allow
   * pre-evaluation, or that need access to context information, can override this method.
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = {
    try {
      val early = new EarlyEvaluationContext(visitor.getConfiguration)
      val item1 = argument(1).evaluateItem(early)
      val lex = item1.getStringValue
      val item0 = argument(0).evaluateItem(early)
      var uri: String = null
      uri = if (item0 == null) "" else item0.getStringValue
      val parts = NameChecker.getQNameParts(lex)
      if (parts(0).length != 0 && !NameChecker.isValidNCName(parts(0))) {
        val err = new XPathException("Malformed prefix in QName: '" + parts(0) + '\'')
        err.setErrorCode("FOCA0002")
        throw err
      }
      Literal.makeLiteral(new QNameValue(parts(0), uri, parts(1), true))
    } catch {
      case e: QNameException ⇒
        dynamicError(e.getMessage, "FOCA0002")
        null
      case err: XPathException ⇒
        err.maybeSetLocation(getSourceLocator)
        throw err
    }
  }

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    var uri: String = null
    uri = if (arg0 == null) null else arg0.getStringValue
    try {
      val lex = argument(1).evaluateItem(context).getStringValue
      val parts = NameChecker.getQNameParts(lex)
      if (parts(0).length != 0 && !NameChecker.isValidNCName(parts(0))) {
        val err = new XPathException("Malformed prefix in QName: '" + parts(0) + '\'')
        err.setErrorCode("FORG0001")
        throw err
      }
      new QNameValue(parts(0), uri, parts(1), true)
    } catch {
      case e: QNameException ⇒
        dynamicError(e.getMessage, "FOCA0002")
        null
      case err: XPathException ⇒
        err.maybeSetLocation(getSourceLocator)
        throw err
    }
  }
}

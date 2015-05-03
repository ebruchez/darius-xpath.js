// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.dom.XMLDOM
import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.ExpressionVisitor
import org.orbeon.darius.xpath.expr.StaticProperty
import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.URI
import org.orbeon.darius.xpath.value.BooleanValue
import org.orbeon.darius.xpath.value.StringValue
import UnparsedText._

import scala.collection.JavaConversions._

object UnparsedText {

  val UNPARSED_TEXT = 0

  val UNPARSED_TEXT_AVAILABLE = 1
}

class UnparsedText(operation: Int) extends SystemFunction {

  this.operation = operation

  def newInstance(): UnparsedText = new UnparsedText(operation)

  var expressionBaseURI: String = null

  def checkArguments(visitor: ExpressionVisitor): Unit = {
    if (expressionBaseURI == null) {
      super.checkArguments(visitor)
      expressionBaseURI = visitor.getStaticContext.getBaseURI
    }
  }

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * @param visitor an expression visitor
   */
  def preEvaluate(visitor: ExpressionVisitor): Expression = this

  def computeSpecialProperties(): Int = {
    super.computeSpecialProperties() & ~StaticProperty.NON_CREATIVE
  }

  /**
   * This method handles evaluation of the function:
   * it returns a StringValue in the case of unparsed-text(), or a BooleanValue
   * in the case of unparsed-text-available(). In the case of unparsed-text-lines()
   * this shouldn't be called, but we deal with it anyway.
   */
  def evaluateItem(context: XPathContext): Item = {
    var content: CharSequence = null
    var result: StringValue = null
    try {
      val hrefVal = argument(0).evaluateItem(context).asInstanceOf[StringValue]
      if (hrefVal == null) {
        return null
      }
      val href = hrefVal.getStringValue
      var encoding: String = null
      if (getNumberOfArguments == 2) {
        encoding = argument(1).evaluateItem(context).getStringValue
      }
      content = readFile(href, expressionBaseURI, encoding, context)
      result = new StringValue(content)
    } catch {
      case err: XPathException ⇒ if (operation == UNPARSED_TEXT_AVAILABLE) {
        return BooleanValue.FALSE
      } else {
        err.maybeSetErrorCode("XTDE1170")
        throw err
      }
    }
    operation match {
      case UNPARSED_TEXT_AVAILABLE ⇒ BooleanValue.TRUE
      case UNPARSED_TEXT ⇒ result
      case _ ⇒ throw new UnsupportedOperationException(operation + "")
    }
  }

  /**
   * Supporting routine to load one external file given a URI (href) and a baseURI
   */
  private def readFile(href: String, 
      baseURI: String, 
      encoding: String, 
      context: XPathContext): CharSequence = {
    val absoluteURI = getAbsoluteURI(href, baseURI)
    XMLDOM.makeHTTPRequest(absoluteURI.toString)
  }

  private def getAbsoluteURI(href: String, baseURI: String): URI = {
    var absoluteURI: URI = null
    absoluteURI = ResolveURI.makeAbsolute(href, baseURI)
    if (absoluteURI.getFragment != null) {
      throw new XPathException("URI for unparsed-text() must not contain a fragment identifier", "XTDE1170")
    }
    absoluteURI
  }
}

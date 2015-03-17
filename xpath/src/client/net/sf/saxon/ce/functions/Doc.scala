// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.{Item, NodeInfo}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue

/**
 * Implement the fn:doc() function - a simplified form of the Document function
 */
class Doc extends SystemFunction {

  def newInstance(): Doc = new Doc()

  private var expressionBaseURI: String = null

  override def checkArguments(visitor: ExpressionVisitor) {
    if (expressionBaseURI == null) {
      super.checkArguments(visitor)
      expressionBaseURI = visitor.getStaticContext.getBaseURI
    }
  }

  /**
   * preEvaluate: this method suppresses compile-time evaluation unless a configuration option has been
   * set to allow early evaluation.
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  override def computeCardinality(): Int = {
    argument(0).getCardinality & ~StaticProperty.ALLOWS_MANY
  }

  /**
   * Evaluate the expression
   * @param context the dynamic evaluation context
   * @return the result of evaluating the expression (a document node)
   * @throws XPathException
   */
  override def evaluateItem(context: XPathContext): Item = doc(context)

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET | 
      StaticProperty.NON_CREATIVE | 
      StaticProperty.SINGLE_DOCUMENT_NODESET
  }

  private def doc(context: XPathContext): NodeInfo = {
    val hrefVal = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (hrefVal == null) {
      return null
    }
    val href = hrefVal.getStringValue
    // ORBEON P2
    ???
//    val item = DocumentFn.makeDoc(href, expressionBaseURI, context, this.getSourceLocator)
//    if (item == null) {
//      dynamicError("Failed to load document " + href, "FODC0002")
//      return null
//    }
//    item
  }
}

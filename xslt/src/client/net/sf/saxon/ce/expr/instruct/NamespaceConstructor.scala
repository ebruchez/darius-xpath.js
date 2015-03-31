// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.event.ReceiverOptions
import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.Whitespace
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A namespace constructor instruction. (xsl:namespace in XSLT 2.0, or namespace{}{} in XQuery 1.1)
 */
class NamespaceConstructor(var name: Expression) extends SimpleNodeConstructor {

  adoptChildExpression(name)

  def simplify(visitor: ExpressionVisitor): Expression = {
    name = visitor.simplify(name)
    super.simplify(visitor)
  }

  def getItemType: ItemType = NodeKindTest.NAMESPACE

  def getCardinality: Int = StaticProperty.EXACTLY_ONE

  protected def promoteInst(offer: PromotionOffer): Unit = {
    if (select != null) {
      select = doPromotion(select, offer)
    }
    name = doPromotion(name, offer)
    super.promoteInst(offer)
  }

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Unit = {
    name = visitor.typeCheck(name, contextItemType)
  }

  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select, name)

  private def evaluatePrefix(context: XPathContext): String = {
    val prefix = Whitespace.trim(name.evaluateAsString(context))
    if (!(prefix.length == 0 || NameChecker.isValidNCName(prefix))) {
      dynamicError("Namespace prefix is invalid: " + prefix, "XTDE0920")
    }
    if (prefix == "xmlns") {
      dynamicError("Namespace prefix 'xmlns' is not allowed", "XTDE0920")
    }
    prefix
  }

  def evaluateNameCode(context: XPathContext): StructuredQName = {
    val prefix = evaluatePrefix(context)
    new StructuredQName("", "", prefix)
  }

  def processValue(value: CharSequence, context: XPathContext): Unit = {
    val prefix = evaluatePrefix(context)
    val uri = value.toString
    checkPrefixAndUri(prefix, uri, context)
    val out = context.getReceiver
    out.namespace(new NamespaceBinding(prefix, uri), ReceiverOptions.REJECT_DUPLICATES)
  }

  /**
   * Evaluate as an expression. We rely on the fact that when these instructions
   * are generated by XQuery, there will always be a valueExpression to evaluate
   * the content
   */
  def evaluateItem(context: XPathContext): Item = {
    val node = super.evaluateItem(context).asInstanceOf[NodeInfo]
    val prefix = node.getLocalPart
    val uri = node.getStringValue
    checkPrefixAndUri(prefix, uri, context)
    node
  }

  private def checkPrefixAndUri(prefix: String, uri: String, context: XPathContext): Unit = {
    if (prefix == "xml" != uri == NamespaceConstant.XML) {
      dynamicError("Namespace prefix 'xml' and namespace uri " + NamespaceConstant.XML + 
        " must only be used together", "XTDE0925")
    }
    if (uri.length == 0) {
      dynamicError("Namespace URI is an empty string", "XTDE0930")
    }
    if (uri == NamespaceConstant.XMLNS) {
      dynamicError("A namespace node cannot have the reserved namespace " + 
        NamespaceConstant.XMLNS, "XTDE0935")
    }
  }
}

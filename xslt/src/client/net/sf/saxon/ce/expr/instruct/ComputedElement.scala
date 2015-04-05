// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.StringValue
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.logging.client.LogConfiguration
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction representing an xsl:element element in an XSLT stylesheet,
 * or a computed element constructor in XQuery. (In both cases, if the element name
 * is expressed as a compile-time expression, then a FixedElement instruction
 * is used instead.)
 * @see FixedElement
 */
class ComputedElement(var elementName: Expression, 
    var namespace: Expression, 
    var nsContext: NamespaceResolver, 
    inheritNamespaces: Boolean) extends ElementCreator {

  this.inheritNamespaces = inheritNamespaces

  adoptChildExpression(elementName)

  adoptChildExpression(namespace)

  if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
    this.AddTraceProperty("name", elementName)
  }

  /**
   * Get the namespace resolver that provides the namespace bindings defined in the static context
   * @return the namespace resolver
   */
  def getNamespaceResolver(): NamespaceResolver = nsContext

  def simplify(visitor: ExpressionVisitor): Expression = {
    elementName = visitor.simplify(elementName)
    namespace = visitor.simplify(namespace)
    super.simplify(visitor)
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    elementName = visitor.typeCheck(elementName, contextItemType)
    var role: RoleLocator = null
    val th = TypeHierarchy.getInstance
    if (!th.isSubType(elementName.getItemType, AtomicType.STRING)) {
      elementName = SystemFunction.makeSystemFunction("string", Array(elementName))
    }
    if (namespace != null) {
      namespace = visitor.typeCheck(namespace, contextItemType)
      role = new RoleLocator(RoleLocator.INSTRUCTION, "element/namespace", 0)
      namespace = TypeChecker.staticTypeCheck(namespace, SequenceType.SINGLE_STRING, backwardsCompatible = false, role)
    }
    if (Literal.isAtomic(elementName)) {
      try {
        val `val` = elementName.asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
        if (`val`.isInstanceOf[StringValue]) {
          val parts = NameChecker.checkQNameParts(`val`.getStringValue)
          if (namespace == null) {
            val prefix = parts(0)
            val uri = getNamespaceResolver.getURIForPrefix(prefix, useDefault = true)
            if (uri == null) {
              val se = new XPathException("Prefix " + prefix + " has not been declared", "XPST0081")
              se.setIsStaticError(true)
              throw se
            }
            namespace = new StringLiteral(uri)
          }
        }
      } catch {
        case e: XPathException ⇒
          val code = e.getErrorCodeLocalPart
          if (code == null || code == "FORG0001") {
            e.setErrorCode("XTDE0820")
          } else if (code == "XPST0081") {
            e.setErrorCode("XTDE0830")
          }
          e.maybeSetLocation(getSourceLocator)
          e.setIsStaticError(true)
          throw e
      }
    }
    super.typeCheck(visitor, contextItemType)
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    elementName = visitor.optimize(elementName, contextItemType)
    super.optimize(visitor, contextItemType)
  }

  def iterateSubExpressions(): Iterator[Expression] = {
    nonNullChildren(content, elementName, namespace)
  }

  /**
   * Offer promotion for subexpressions. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *              expressions that don't depend on the context to an outer level in
   *              the containing expression
   * @throws client.net.sf.saxon.ce.trans.XPathException if any error is detected
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    elementName = doPromotion(elementName, offer)
    if (namespace != null) {
      namespace = doPromotion(namespace, offer)
    }
    super.promoteInst(offer)
  }

  /**
   * Callback from the superclass ElementCreator to get the nameCode
   * for the element name
   *
   *
   * @param context The evaluation context (not used)
   * @param copiedNode
   * @return the name code for the element name
   */
  def getNameCode(context: XPathContext, copiedNode: NodeInfo): StructuredQName = {
    var prefix: String = null
    var localName: String = null
    var uri: String = null
    val nameValue = elementName.evaluateItem(context).asInstanceOf[AtomicValue]
    if (nameValue == null) {
      dynamicError("Invalid element name (empty sequence)", "XTDE0820")
    }
    if (nameValue.isInstanceOf[StringValue]) {
      var rawName = nameValue.getStringValue
      rawName = Whitespace.trimWhitespace(rawName)
      try {
        val parts = NameChecker.getQNameParts(rawName)
        prefix = parts(0)
        localName = parts(1)
      } catch {
        case err: QNameException ⇒
          var message = "Invalid element name. " + err.getMessage
          if (rawName.length == 0) {
            message = "Supplied element name is a zero-length string"
          }
          dynamicError(message, "XTDE0820")
      }
    } else {
      dynamicError("Computed element name has incorrect type", "XTDE0820")
    }
    if (namespace == null && uri == null) {
      uri = nsContext.getURIForPrefix(prefix, useDefault = true)
      if (uri == null) {
        dynamicError("Undeclared prefix in element name: " + prefix, "XTDE0830")
      }
    } else {
      if (uri == null) {
        uri = namespace.evaluateAsString(context).toString
      }
      if (uri.length == 0) {
        prefix = ""
      }
      if (prefix == "xmlns") {
        prefix = "x-xmlns"
      }
    }
    if (uri == NamespaceConstant.XMLNS) {
      dynamicError("Cannot create element in namespace " + uri, "XTDE0835")
    }
    if (uri == NamespaceConstant.XML != prefix == "xml") {
      var message: String = null
      message = if (prefix == "xml") "When the prefix is 'xml', the namespace URI must be " + 
        NamespaceConstant.XML else "When the namespace URI is " + NamespaceConstant.XML + 
        ", the prefix must be 'xml'"
      dynamicError(message, "XTDE0835")
    }
    new StructuredQName(prefix, uri, localName)
  }

  def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String = getBaseURI

  /**
   * Callback to output namespace nodes for the new element.
   *
   *
   * @param context The execution context
   * @param out     the Receiver where the namespace nodes are to be written
   * @param nameCode
   * @param copiedNode
   * @throws XPathException
   */
  protected def outputNamespaceNodes(context: XPathContext, 
      out: Receiver, 
      nameCode: StructuredQName, 
      copiedNode: NodeInfo): Unit = {
  }
}

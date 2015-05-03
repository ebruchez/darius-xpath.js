// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.functions.SystemFunction
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.AtomicType
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.`type`.TypeHierarchy
import org.orbeon.darius.xpath.value.SequenceType
import org.orbeon.darius.xpath.value.StringValue
import org.orbeon.darius.xpath.value.Whitespace
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction derived from an xsl:attribute element in stylesheet, or from
 * an attribute constructor in XQuery, in cases where the attribute name is not known
 * statically
 */
class ComputedAttribute(var attributeName: Expression, var namespace: Expression, var nsContext: NamespaceResolver)
    extends AttributeCreator {

  adoptChildExpression(attributeName)

  adoptChildExpression(namespace)

  /**
   * Get the namespace resolver used to resolve any prefix in the name of the attribute
   * @return the namespace resolver if one has been saved; or null otherwise
   */
  def getNamespaceResolver(): NamespaceResolver = nsContext

  /**
   * Get the static type of this expression
   * @return the static type of the item returned by this expression
   */
  def getItemType(): ItemType = NodeKindTest.ATTRIBUTE

  /**
   * Get the static cardinality of this expression
   * @return the static cardinality (exactly one)
   */
  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  def computeSpecialProperties(): Int = {
    super.computeSpecialProperties() | StaticProperty.SINGLE_DOCUMENT_NODESET
  }

  def simplify(visitor: ExpressionVisitor): Expression = {
    attributeName = visitor.simplify(attributeName)
    namespace = visitor.simplify(namespace)
    super.simplify(visitor)
  }

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Unit = {
    val env = visitor.getStaticContext
    attributeName = visitor.typeCheck(attributeName, contextItemType)
    adoptChildExpression(attributeName)
    var role: RoleLocator = null
    val th = TypeHierarchy.getInstance
    if (!th.isSubType(attributeName.getItemType, AtomicType.STRING)) {
      attributeName = SystemFunction.makeSystemFunction("string", Array(attributeName))
    }
    if (namespace != null) {
      visitor.typeCheck(namespace, contextItemType)
      adoptChildExpression(namespace)
      role = new RoleLocator(RoleLocator.INSTRUCTION, "attribute/namespace", 0)
      namespace = TypeChecker.staticTypeCheck(namespace, SequenceType.SINGLE_STRING, backwardsCompatible = false, role)
    }
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    attributeName = visitor.optimize(attributeName, contextItemType)
    if (namespace != null) {
      namespace = visitor.optimize(namespace, contextItemType)
    }
    val exp = super.optimize(visitor, contextItemType)
    if (exp != this) {
      return exp
    }
    if (attributeName.isInstanceOf[Literal] && 
      (namespace == null || namespace.isInstanceOf[Literal])) {
      val context = new EarlyEvaluationContext(visitor.getConfiguration)
      val nc = evaluateNameCode(context)
      val fa = new FixedAttribute(nc)
      fa.setSelect(getContentExpression, visitor.getConfiguration)
      return fa
    }
    this
  }

  /**
   * Get the subexpressions of this expression
   * @return an iterator over the subexpressions
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    nonNullChildren(select, attributeName, namespace)
  }

  /**
   * Offer promotion for subexpressions. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *     expressions that don't depend on the context to an outer level in
   *     the containing expression
   * @exception XPathException if any error is detected
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    attributeName = doPromotion(attributeName, offer)
    if (namespace != null) {
      namespace = doPromotion(namespace, offer)
    }
    super.promoteInst(offer)
  }

  /**
   * Determine the name to be used for the attribute, as a StructuredQName
   *
   * @param context Dynamic evaluation context
   * @return the StructuredQName for the attribute name
   * @throws XPathException
   */
  def evaluateNameCode(context: XPathContext): StructuredQName = {
    val nameValue = attributeName.evaluateItem(context)
    var prefix: String = null
    var localName: String = null
    var uri: String = null
    if (nameValue.isInstanceOf[StringValue]) {
      var rawName = nameValue.getStringValue
      rawName = Whitespace.trimWhitespace(rawName)
      try {
        val parts = NameChecker.getQNameParts(rawName)
        prefix = parts(0)
        localName = parts(1)
      } catch {
        case err: QNameException ⇒ dynamicError("Invalid attribute name: " + rawName, "XTDE0850")
      }
      if (rawName.toString == "xmlns") {
        if (namespace == null) {
          dynamicError("Invalid attribute name: " + rawName, "XTDE0855")
        }
      }
      if (prefix == "xmlns") {
        if (namespace == null) {
          dynamicError("Invalid attribute name: " + rawName, "XTDE0860")
        } else {
          prefix = ""
        }
      }
    } else {
      typeError("Attribute name must be either a string or a QName", "XPTY0004")
    }
    if (namespace == null && uri == null) {
      if (prefix.length == 0) {
        uri = ""
      } else {
        uri = nsContext.getURIForPrefix(prefix, useDefault = false)
        if (uri == null) {
          dynamicError("Undeclared prefix in attribute name: " + prefix, "XTDE0860")
        }
      }
    } else {
      if (uri == null) {
        uri = namespace.evaluateAsString(context).toString
      }
      if (uri.length == 0) {
        prefix = ""
      } else {
        if (prefix.length == 0) {
          prefix = "ns0"
          val prefixes = nsContext.iteratePrefixes()
          while (prefixes.hasNext) {
            val p = prefixes.next()
            if (nsContext.getURIForPrefix(p, useDefault = false) == uri) {
              prefix = p
              //break
            }
          }
        }
      }
    }
    if (uri == NamespaceConstant.XMLNS) {
      dynamicError("Cannot create attribute in namespace " + uri, "XTDE0835")
    }
    new StructuredQName(prefix, uri, localName)
  }
}

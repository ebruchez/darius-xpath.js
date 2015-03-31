// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.StringFn
import client.net.sf.saxon.ce.om.{Item, SequenceIterator, StructuredQName}
import client.net.sf.saxon.ce.orbeon.{Configuration, Iterator}
import client.net.sf.saxon.ce.pattern.NodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.tree.util.Orphan
import client.net.sf.saxon.ce.value.Cardinality

/**
 * Common superclass for XSLT instructions whose content template produces a text
 * value: xsl:attribute, xsl:comment, xsl:processing-instruction, xsl:namespace,
 * and xsl:text, and their XQuery equivalents
 */
abstract class SimpleNodeConstructor extends Instruction {

  protected var select: Expression = null

  /**
   * Set the select expression: the value of this expression determines the string-value of the node
   * @param select the expression that computes the string value of the node
   * @param config the Saxon configuration (used for example to do early validation of the content
   * of an attribute against the schema-defined type)
   * @throws XPathException
   */
  def setSelect(select: Expression, config: Configuration): Unit = {
    this.select = select
    adoptChildExpression(select)
  }

  /**
   * Get the expression that determines the string value of the constructed node
   * @return the select expression
   */
  def getContentExpression: Expression = select

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true.
   */
  override def createsNewNodes(): Boolean = true

  /**
   * Get the cardinality of the sequence returned by evaluating this instruction
   * @return the static cardinality
   */
  override def computeCardinality(): Int = select.getCardinality

  override def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    this
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  override def computeSpecialProperties(): Int = {
    super.computeSpecialProperties() | StaticProperty.SINGLE_DOCUMENT_NODESET
  }

  /**
   * Method to perform type-checking specific to the kind of instruction
   * @param visitor an expression visitor
   * @param contextItemType the static type of the context item
   * @throws XPathException
   */
  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Unit

  /**
   * The typeCheck() method is called in XQuery, where node constructors
   * are implemented as Expressions. In this case the required type for the
   * select expression is a single string.
   * @param visitor an expression visitor
   * @return the rewritten expression
   * @throws XPathException if any static errors are found in this expression
   * or any of its children
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    localTypeCheck(visitor, contextItemType)
    if (select != null) {
      val th = TypeHierarchy.getInstance
      select = visitor.typeCheck(select, contextItemType)
      if (select.isInstanceOf[ValueOf]) {
        val valSelect = select.asInstanceOf[ValueOf].getContentExpression
        if (th.isSubType(valSelect.getItemType, AtomicType.STRING) && 
          !Cardinality.allowsMany(valSelect.getCardinality)) {
          select = valSelect
        }
      }
      if (select.isInstanceOf[StringFn]) {
        val fn = select.asInstanceOf[StringFn]
        val arg = fn.getArguments(0)
        if (arg.getItemType == AtomicType.UNTYPED_ATOMIC && !Cardinality.allowsMany(arg.getCardinality)) {
          select = arg
        }
      } else if (select.isInstanceOf[CastExpression] && 
        select.asInstanceOf[CastExpression].getTargetType == AtomicType.STRING) {
        val arg = select.asInstanceOf[CastExpression].getBaseExpression
        if (arg.getItemType == AtomicType.UNTYPED_ATOMIC && !Cardinality.allowsMany(arg.getCardinality)) {
          select = arg
        }
      }
      adoptChildExpression(select)
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.optimize(select, contextItemType)
    adoptChildExpression(select)
    this
  }

  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select)

  /**
   * Process this instruction
   * @param context the dynamic context of the transformation
   * @return a TailCall to be executed by the caller, always null for this instruction
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    val value = select.evaluateAsString(context)
    processValue(value, context)
    null
  }

  /**
   * Process the value of the node, to create the new node.
   * @param value the string value of the new node
   * @param context the dynamic evaluation context
   * @throws XPathException
   */
  def processValue(value: CharSequence, context: XPathContext): Unit

  /**
   * Evaluate as an expression.
   */
  override def evaluateItem(context: XPathContext): Item = {
    val contentItem = select.evaluateItem(context)
    if (contentItem == null) {
      return null
    }
    var content = contentItem.getStringValue
    content = checkContent(content, context)
    val th = TypeHierarchy.getInstance
    val o = new Orphan()
    o.setNodeKind(getItemType.asInstanceOf[NodeTest].getRequiredNodeKind)
    o.setStringValue(content)
    o.setNodeName(evaluateNameCode(context))
    o
  }

  /**
   * Check the content of the node, and adjust it if necessary. The checks depend on the node kind.
   * @param data the supplied content
   * @param context the dynamic context
   * @return the original content, unless adjustments are needed
   * @throws XPathException if the content is invalid
   */
  protected def checkContent(data: String, context: XPathContext): String = data

  /**
   * Run-time method to compute the name of the node being constructed. This is overridden
   * for nodes that have a name. The default implementation returns -1, which is suitable for
   * unnamed nodes such as comments
   *
   * @param context the XPath dynamic evaluation context
   * @return the name pool nameCode identifying the name of the constructed node
   * @throws XPathException if any failure occurs
   */
  def evaluateNameCode(context: XPathContext): StructuredQName = null

  override def iterate(context: XPathContext): SequenceIterator = {
    SingletonIterator.makeIterator(evaluateItem(context))
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
   * @throws XPathException if any error is detected
   */
  override protected def promoteInst(offer: PromotionOffer): Unit = {
    if (select != null) {
      select = doPromotion(select, offer)
    }
    super.promoteInst(offer)
  }
}

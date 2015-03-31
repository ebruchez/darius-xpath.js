// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.expr.PromotionOffer._
import client.net.sf.saxon.ce.expr.sort.DocumentSorter
import client.net.sf.saxon.ce.functions.Reverse
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.value.{Cardinality, SequenceType}

object PromotionOffer {

  /**
   * FOCUS_INDEPENDENT requests promotion of all non-trivial subexpressions that don't depend on the
   * focus. This is typically used to extract subexpressions from a filter predicate. The offer is
   * optional - each subexpression can decide whether it's worth the trouble of promoting itself.
   * The offer is normally passed on to subexpressions, except subexpressions that are evaluated
   * with a different focus
   */
  val FOCUS_INDEPENDENT = 10

  /**
   * RANGE_INDEPENDENT requests promotion of all non-trivial subexpressions that don't depend on a
   * specified range variable. This is typically used to extract subexpressions from the action of
   * a for expression or the condition of a some/every quantified expression. The offer is
   * optional - each subexpression can decide whether it's worth the trouble of promoting itself.
   * The offer is normally passed on to subexpressions, except subexpressions that are evaluated
   * with a different focus or a different range variable, because these may have other dependencies
   * that prevent their promotion.
   */
  val RANGE_INDEPENDENT = 11

  /**
   * UNORDERED indicates that the containing expression does not require the results
   * to be delivered in any particular order. The boolean mustEliminateDuplicates
   * is set if duplicate items in the result are not allowed.
   */
  val UNORDERED = 13

  /**
   * REPLACE_CURRENT causes calls to the XSLT current() function to be replaced by
   * reference to a variable. The variable binding is the single member of the array bindingList
   */
  val REPLACE_CURRENT = 14
}

/**
 * PromotionOffer is an object used transiently during compilation of an expression. It contains
 * information passed by a containing expression to its subexpressions, when looking for subexpressions
 * that can be promoted to a higher level because they are not dependent on the context established
 * by the containing expression. The object is also used to pass back return information when the
 * promotion actually takes place.
 */
class PromotionOffer {

  /**
   * The expression visitor in use
   */
  var visitor: ExpressionVisitor = _

  /**
   * action is one of the possible promotion actions, FOCUS_INDEPENDENT, RANGE_INDEPENDENT,
   * INLINE_VARIABLE_REFERENCES, UNORDERED, EXTRACT_GLOBAL_VARIABLES
   */
  var action: Int = _

  /**
   * In the case of FOCUS_INDEPENDENT, "promoteDocumentDependent" is a boolean that, when set to
   * true, indicates that it is safe to promote a subexpression that depends on the context document
   * but not on other aspects of the focus. This is the case, for example, in a filter expression when
   * it is known that all the nodes selected by the expression will be in the same document - as happens
   * when the filter is applied to a path expression. This allows subexpressions such as key() to be
   * promoted
   */
  var promoteDocumentDependent: Boolean = false

  /**
   * In the case of FOCUS_INDEPENDENT, "promoteXSLTFunctions" is a boolean that, when set to true, indicates
   * that it is safe to promote XSLT functions such as current(). This flag is set when rewriting XPath expressions
   * and is unset when rewriting XSLT templates.
   */
  var promoteXSLTFunctions: Boolean = true

  /**
   * In the case of UNORDERED, "retainAllNodes" is a boolean that is set to
   * true if the nodes can be delivered in any order so long as the right number of nodes
   * are delivered. If this boolean is false, the caller doesn't care whether duplicate nodes
   * are retained or whether they are eliminated.
   */
  var retainAllNodes: Boolean = true

  /**
   * In the case of RANGE_INDEPENDENT, "binding" identifies the range variables whose dependencies
   * we are looking for. For INLINE_VARIABLE_REFERENCES it is a single Binding that we are aiming to inline
   */
  var bindingList: Array[Binding] = _

  /**
   * When a promotion offer is made, containingExpression identifies the level to which the promotion
   * should occur. When a subexpression is promoted, an expression of the form let $VAR := SUB return ORIG
   * is created, and this replaces the original containingExpression within the PromotionOffer.
   */
  var containingExpression: Expression = _

  /**
   * Flag that is set if the offer has been accepted, that is, if the expression has changed
   */
  var accepted: Boolean = false

  /**
   * Method to test whether a subexpression qualifies for promotion, and if so, to
   * accept the promotion.
   * @param parent
   * @param child the subexpression in question
   * @return if promotion was done, returns the expression that should be used in place
   * of the child expression. If no promotion was done, returns null. If promotion is
   * determined not to be necessary for this subtree, returns the supplied child expression
   * unchanged
   */
  def accept(parent: Expression, child: Expression): Expression = action match {
    case RANGE_INDEPENDENT ⇒
      val properties = child.getSpecialProperties
      if (((properties & StaticProperty.NON_CREATIVE) != 0) && 
        !ExpressionTool.dependsOnVariable(child, bindingList) && 
        (child.getDependencies & StaticProperty.HAS_SIDE_EFFECTS) ==
        0) {
        promote(parent, child)
      } else
        null
    case FOCUS_INDEPENDENT ⇒
      val dependencies = child.getDependencies
      val properties = child.getSpecialProperties
      if (!promoteXSLTFunctions && 
        ((dependencies & StaticProperty.DEPENDS_ON_XSLT_CONTEXT) != 0)) {
        return null
      }
      if (ExpressionTool.dependsOnVariable(child, bindingList)) {
        return null
      }
      if ((dependencies & StaticProperty.HAS_SIDE_EFFECTS) != 0) {
        return null
      }
      if ((dependencies & StaticProperty.DEPENDS_ON_FOCUS) == 0 && (properties & StaticProperty.NON_CREATIVE) != 0) {
        promote(parent, child)
      } else if (promoteDocumentDependent &&
          (dependencies & StaticProperty.DEPENDS_ON_NON_DOCUMENT_FOCUS) == 0 &&
          (properties & StaticProperty.NON_CREATIVE) != 0) {
        promote(parent, child)
      } else
        null
    case REPLACE_CURRENT ⇒
      null
//ORBEON XSLT
//      if (child.isInstanceOf[Current]) {
//        val `var` = new LocalVariableReference(containingExpression.asInstanceOf[Assignation])
//        ExpressionTool.copyLocationInfo(child, `var`)
//        return `var`
//      } else if (!ExpressionTool.callsFunction(child, Current.FN_CURRENT)) {
//        return child
//      }
    case UNORDERED ⇒
      if (child.isInstanceOf[Reverse]) {
        child.asInstanceOf[Reverse].getArguments(0)
      } else if (child.isInstanceOf[DocumentSorter] && !retainAllNodes) {
        child.asInstanceOf[DocumentSorter].getBaseExpression
      } else
        null
    case _ ⇒
        throw new UnsupportedOperationException("Unknown promotion action " + action)
  }

  /**
   * Method to promote a subexpression. A LetExpression is created which binds the child expression
   * to a system-created variable, and then returns the original expression, with the child expression
   * replaced by a reference to the variable.
   * @param child the expression to be promoted
   * @return the expression that results from the promotion, if any took place
   */
  private def promote(parent: Expression, child: Expression): Expression = {
    val let = new LetExpression()
    let.setVariableQName(new StructuredQName("zz", NamespaceConstant.SAXON, "zz" + let.hashCode))
    val `type` = SequenceType.makeSequenceType(child.getItemType, child.getCardinality)
    let.setRequiredType(`type`)
    ExpressionTool.copyLocationInfo(containingExpression, let)
    let.setSequence(child)
    let.setEvaluationMode(if (Cardinality.allowsMany(child.getCardinality)) ExpressionTool.ITERATE_AND_MATERIALIZE else ExpressionTool.CALL_EVALUATE_ITEM)
    let.setAction(containingExpression)
    let.adoptChildExpression(containingExpression)
    containingExpression = let
    accepted = true
    val `var` = new LocalVariableReference(let)
    val properties = child.getSpecialProperties & StaticProperty.NOT_UNTYPED
    `var`.setStaticType(`type`, null, properties)
    ExpressionTool.copyLocationInfo(containingExpression, `var`)
    `var`
  }
}

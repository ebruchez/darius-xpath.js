// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.LogConfiguration
import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType}
import client.net.sf.saxon.ce.om.{Item, NodeInfo, SequenceIterator}
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.SingletonIterator

/**
 * A node set expression that will always return zero or one nodes
 */
abstract class SingleNodeExpression extends Expression {

  /**
   * Type-check the expression.
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (contextItemType == null || contextItemType.isInstanceOf[AtomicType]) {
      var message = ""
      var code = ""
      if (LogConfiguration.loggingIsEnabled()) {
        if (contextItemType == null) {
          code = "XPDY0002"
          message = noContextMessage() + ": the context item is undefined"
        } else {
          code = "XPTY0020"
          message = noContextMessage() + ": the context item is an atomic value"
        }
      }
      typeError(visitor, message, code)
    }
    this
  }

  /**
   * Customize the error message on type checking
   */
  protected def noContextMessage(): String

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = typeCheck(visitor, contextItemType)

  /**
   * Specify that the expression returns a singleton
   */
  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_ONE

  /**
   * Determine the data type of the items returned by this expression
   * @return Type.NODE
   */
  def getItemType: ItemType = AnyNodeTest.getInstance

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as StaticProperty.VARIABLES and
   * StaticProperty.CURRENT_NODE
   */
  override def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  override def computeSpecialProperties(): Int = {
    StaticProperty.ORDERED_NODESET | StaticProperty.CONTEXT_DOCUMENT_NODESET | 
      StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.NON_CREATIVE
  }

  /**
   * Get the single node to which this expression refers. Returns null if the node-set is empty
   */
  def getNode(context: XPathContext): NodeInfo

  /**
   * Evaluate the expression in a given context to return an iterator
   * @param context the evaluation context
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    SingletonIterator.makeIterator(getNode(context))
  }

  override def evaluateItem(context: XPathContext): Item = getNode(context)
}

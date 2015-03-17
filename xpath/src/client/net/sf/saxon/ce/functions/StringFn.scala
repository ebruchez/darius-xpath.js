// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.SimpleNodeConstructor
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.StringValue

/**
 * Implement XPath function string()
 */
class StringFn extends SystemFunction {

  def newInstance(): StringFn = new StringFn()

  /**
   * Simplify and validate.
   * This is a pure function so it can be simplified in advance if the arguments are known
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    argument(0).setFlattened(true)
    simplifyArguments(visitor)
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   * @param visitor         an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    val th = TypeHierarchy.getInstance
    if (th.isSubType(argument(0).getItemType, AtomicType.STRING) && 
      argument(0).getCardinality == StaticProperty.EXACTLY_ONE) {
      return argument(0)
    }
    if (argument(0).isInstanceOf[SimpleNodeConstructor]) {
      return argument(0).asInstanceOf[SimpleNodeConstructor].getContentExpression
    }
    this
  }

  /**
   * Evaluate the function
   */
  override def evaluateItem(c: XPathContext): Item = {
    try {
      val arg = argument(0).evaluateItem(c)
      if (arg == null) {
        StringValue.EMPTY_STRING
      } else if (arg.isInstanceOf[StringValue] && 
        arg.asInstanceOf[StringValue].getItemType == AtomicType.STRING) {
        arg
      } else {
        StringValue.makeStringValue(arg.getStringValue)
      }
    } catch {
      case e: UnsupportedOperationException => {
        typeError(e.getMessage, "FOTY0014")
        null
      }
    }
  }
}

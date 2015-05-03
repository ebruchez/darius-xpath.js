// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.value._

/**
 * Implement the XPath string-length() function
 */
class StringLength extends SystemFunction {

  def newInstance(): StringLength = new StringLength()

  /**
   * Determine the intrinsic dependencies of an expression, that is, those which are not derived
   * from the dependencies of its subexpressions. For example, position() has an intrinsic dependency
   * on the context position, while (position()+1) does not. The default implementation
   * of the method returns 0, indicating "no dependencies".
   *
   * @return a set of bit-significant flags identifying the "intrinsic"
   *         dependencies. The flags are documented in class client.net.sf.saxon.ce.value.StaticProperty
   */
  override def getIntrinsicDependencies: Int = {
    var d = super.getIntrinsicDependencies
    if (argument.length == 0) {
      d |= StaticProperty.DEPENDS_ON_CONTEXT_ITEM
    }
    d
  }

  /**
   * Pre-evaluate a function at compile time. Functions that do not allow
   * pre-evaluation, or that need access to context information, can override this method.
   * @param visitor an expression visitor
   * @return the expression, either unchanged, or pre-evaluated
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = {
    if (argument.length == 0) {
      this
    } else {
      Literal.makeLiteral(evaluateItem(new EarlyEvaluationContext(visitor.getConfiguration)))
    }
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (argument.length == 0 && contextItemType == null) {
      typeError("The context item for string-length() is undefined", "XPDY0002")
    }
    super.typeCheck(visitor, contextItemType)
  }

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = {
    var sv: AtomicValue = null
    if (argument.length == 0) {
      val contextItem = c.getContextItem
      if (contextItem == null) {
        dynamicError("The context item for string-length() is not set", "XPDY0002")
        return null
      }
      sv = StringValue.makeStringValue(contextItem.getStringValue)
    } else {
      sv = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    }
    if (sv == null) {
      return IntegerValue.ZERO
    }
    sv match {
      case value: StringValue ⇒
        new IntegerValue(value.getStringLength)
      case _ ⇒
        val s = sv.getStringValue
        new IntegerValue(StringValue.getStringLength(s))
    }
  }
}

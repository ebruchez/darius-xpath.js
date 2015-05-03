// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.om.NodeInfo

/**
 * Class ParentNodeExpression represents the XPath expression ".." or "parent::node()"
 */
class ParentNodeExpression extends SingleNodeExpression {

  /**
   * Customize the error message on type checking
   */
  protected def noContextMessage(): String = {
    "Cannot select the parent of the context node"
  }

  /**
   * Return the node selected by this SingleNodeExpression
   * @param context The context for the evaluation
   * @return the parent of the current node defined by the context
   */
  def getNode(context: XPathContext): NodeInfo = {
    val item = context.getContextItem
    if (item == null) {
      dynamicError("The context item is not set", "XPDY0002")
    }
    item match {
      case info: NodeInfo ⇒
        info.getParent
      case _ ⇒
        dynamicError("The context item for the parent axis (..) is not a node", "XPTY0020")
        null
    }
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  private def copy(): Expression = new ParentNodeExpression()

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    other.isInstanceOf[ParentNodeExpression]
  }

  /**
   * get HashCode for comparing two expressions
   */
  override def hashCode(): Int = "ParentNodeExpression".hashCode

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString: String = ".."
}

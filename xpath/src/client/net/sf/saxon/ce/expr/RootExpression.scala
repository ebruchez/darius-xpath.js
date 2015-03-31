// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.pattern.NodeKindTest

/**
 * An expression whose value is always a set of nodes containing a single node,
 * the document root. This corresponds to the XPath Expression "/", including the implicit
 * "/" at the start of a path expression with a leading "/".
 */
class RootExpression extends SingleNodeExpression {

  /**
   * Customize the error message on type checking
   */
  protected def noContextMessage(): String = {
    "Leading '/' cannot select the root node of the tree containing the context item"
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = other.isInstanceOf[RootExpression]

  /**
   * Specify that the expression returns a singleton
   */
  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Determine the data type of the items returned by this expression
   *
   * @return Type.NODE
   */
  override def getItemType(): ItemType = NodeKindTest.DOCUMENT

  /**
   * get HashCode for comparing two expressions
   */
  override def hashCode(): Int = "RootExpression".hashCode

  /**
   * Return the first element selected by this Expression
   * @param context The evaluation context
   * @return the NodeInfo of the first selected element, or null if no element
   * is selected
   */
  def getNode(context: XPathContext): NodeInfo = {
    val current = context.getContextItem
    if (current == null) {
      dynamicError("Finding root of tree: the context item is undefined", "XPDY0002")
    }
    if (current.isInstanceOf[NodeInfo]) {
      val doc = current.asInstanceOf[NodeInfo].getDocumentRoot
      if (doc == null) {
        dynamicError("The root of the tree containing the context item is not a document node", "XPDY0050")
      }
      return doc
    }
    typeError("Finding root of tree: the context item is not a node", "XPTY0020")
    null
  }

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as StaticProperty.VARIABLES and
   * StaticProperty.CURRENT_NODE
   */
  override def getIntrinsicDependencies(): Int = {
    StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT
  }

  /**
   * Copy an expression. This makes a deep copy.
   * @return the copy of the original expression
   */
  private def copy(): Expression = new RootExpression()

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = "(/)"
}

package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class represents the expression "(dot)", which always returns the context item.
 * This may be a AtomicValue or a Node.
 */
class ContextItemExpression extends Expression {

  var itemType: ItemType = Type.ITEM_TYPE

  /**
   * Create a clone copy of this expression
   * @return a copy of this expression
   */
  private def copy(): Expression = {
    val cie2 = new ContextItemExpression()
    cie2.itemType = itemType
    cie2
  }

  protected def getErrorCodeForUndefinedContext(): String = "XPDY0002"

  /**
   * Type-check the expression.
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (contextItemType == null) {
      typeError("The context item is undefined at this point", getErrorCodeForUndefinedContext)
    }
    itemType = contextItemType
    this
  }

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
   *                        {@link client.net.sf.saxon.ce.type.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = typeCheck(visitor, contextItemType)

  /**
   * Determine the item type
   */
  def getItemType(): ItemType = itemType

  /**
   * Get the static cardinality
   */
  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Determine the special properties of this expression
   * @return the value {@link StaticProperty#NON_CREATIVE}
   */
  def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    (other.isInstanceOf[ContextItemExpression])
  }

  /**
   * get HashCode for comparing two expressions
   */
  override def hashCode(): Int = "ContextItemExpression".hashCode

  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  /**
   * Iterate over the value of the expression
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val item = context.getContextItem
    if (item == null) {
      dynamicError("The context item is not set", getErrorCodeForUndefinedContext)
    }
    SingletonIterator.makeIterator(item)
  }

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val item = context.getContextItem
    if (item == null) {
      dynamicError("The context item is not set", getErrorCodeForUndefinedContext)
    }
    item
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = "."
}

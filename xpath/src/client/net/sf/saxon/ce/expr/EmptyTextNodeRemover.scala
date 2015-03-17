package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class performs the first phase of processing in "constructing simple content":
 * it takes an input sequence, eliminates empty text nodes, and combines adjacent text nodes
 * into one.
 * @since 9.3
 */
class EmptyTextNodeRemover(p0: Expression) extends UnaryExpression(p0) {

  /**
   * Determine the data type of the expression, if possible. The default
   * implementation for unary expressions returns the item type of the operand
   * @return the item type of the items in the result sequence, insofar as this
   *         is known statically.
   */
  override def getItemType(): ItemType = getBaseExpression.getItemType

  override def computeCardinality(): Int = {
    getBaseExpression.getCardinality | StaticProperty.ALLOWS_ZERO
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered.
   */
  def getImplementationMethod(): Int = Expression.ITERATE_METHOD

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation handles iteration for expressions that
   * return singleton values: for non-singleton expressions, the subclass must
   * provide its own implementation.
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *         of the expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any dynamic error occurs evaluating the
   *          expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val map = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        if (item.isInstanceOf[NodeInfo] && 
          item.asInstanceOf[NodeInfo].getNodeKind == Type.TEXT && 
          item.getStringValue.length == 0) {
          return null
        } else {
          return item
        }
      }
    }
    new ItemMappingIterator(getBaseExpression.iterate(context), map)
  }
}

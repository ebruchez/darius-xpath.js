package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.Cardinality
import client.net.sf.saxon.ce.value.SequenceExtent
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An AtomicSequenceConverter is an expression that performs a cast on each member of
 * a supplied sequence
 */
class AtomicSequenceConverter(sequence: Expression, var requiredItemType: AtomicType)
    extends UnaryExpression(sequence) {

  ExpressionTool.copyLocationInfo(sequence, this)

  /**
   * Simplify an expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    if (operand.isInstanceOf[Literal]) {
      val `val` = SequenceExtent.makeSequenceExtent(iterate(new EarlyEvaluationContext(visitor.getConfiguration)))
      return Literal.makeLiteral(`val`)
    }
    this
  }

  /**
   * Type-check the expression
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    val th = TypeHierarchy.getInstance
    if (th.isSubType(operand.getItemType, requiredItemType)) {
      operand
    } else if (!Cardinality.allowsMany(operand.getCardinality)) {
      val cast = new CastExpression(operand, requiredItemType, (operand.getCardinality & StaticProperty.ALLOWS_ZERO) != 
        0)
      ExpressionTool.copyLocationInfo(this, cast)
      cast
    } else {
      this
    }
  }

  /**
   * Determine the special properties of this expression
   * @return {@link StaticProperty#NON_CREATIVE}.
   */
  def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Iterate over the sequence of values
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val base = operand.iterate(context)
    val converter = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        return item.asInstanceOf[AtomicValue].convert(requiredItemType)
          .asAtomic()
      }
    }
    new ItemMappingIterator(base, converter, true)
  }

  /**
   * Evaluate as an Item. This should only be called if the AtomicSequenceConverter has cardinality zero-or-one
   */
  def evaluateItem(context: XPathContext): Item = {
    val item = operand.evaluateItem(context)
    if (item == null) return null
    item.asInstanceOf[AtomicValue].convert(requiredItemType)
      .asAtomic()
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   * or Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = requiredItemType

  /**
   * Determine the static cardinality of the expression
   */
  def computeCardinality(): Int = operand.getCardinality

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    super == other && 
      requiredItemType == 
      other.asInstanceOf[AtomicSequenceConverter].requiredItemType
  }

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = {
    super.hashCode ^ requiredItemType.hashCode
  }
}

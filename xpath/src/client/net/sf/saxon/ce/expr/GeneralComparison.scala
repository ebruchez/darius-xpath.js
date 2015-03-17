package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.expr.sort.AtomicComparer
import client.net.sf.saxon.ce.expr.sort.CodepointCollator
import client.net.sf.saxon.ce.expr.sort.GenericAtomicComparer
import client.net.sf.saxon.ce.functions.NumberFn
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value._
import client.net.sf.saxon.ce.value.StringValue
import GeneralComparison._
//remove if not needed
import scala.collection.JavaConversions._

object GeneralComparison {

  /**
   * Atomize a sequence
   * @param v the sequence to be atomized
   * @return an iterator over the atomized sequence
   * @throws XPathException if atomization fails
   */
  private def atomize(v: Sequence): SequenceIterator = {
    if (v.isInstanceOf[AtomicValue]) {
      SingletonIterator.makeIterator(v.asInstanceOf[AtomicValue])
    } else {
      Atomizer.getAtomizingIterator(v.iterate())
    }
  }

  /**
   * Return the singleton form of the comparison operator, e.g. FEQ for EQUALS, FGT for GT
   * @param op the general comparison operator, for example Token.EQUALS
   * @return the corresponding value comparison operator, for example Token.FEQ
   */
  private def getSingletonOperator(op: Int): Int = op match {
    case Token.EQUALS => Token.FEQ
    case Token.GE => Token.FGE
    case Token.NE => Token.FNE
    case Token.LT => Token.FLT
    case Token.GT => Token.FGT
    case Token.LE => Token.FLE
    case _ => op
  }
}

/**
 * GeneralComparison: a boolean expression that compares two expressions
 * for equals, not-equals, greater-than or less-than. This implements the operators
 * =, !=, <, >, etc. This version of the class implements general comparisons
 * in both XPath 1.0 backwards compatibility mode and 2.0 mode. As a result,
 * no static type checking is done.
 */
class GeneralComparison(p0: Expression, op: Int, p1: Expression) extends BinaryExpression(p0, op, p1) {

  private var comparer: AtomicComparer = _

  private var backwardsCompatible: Boolean = _

  /**
   * Determine the static cardinality. Returns [1..1]
   */
  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def simplify(visitor: ExpressionVisitor): Expression = {
    backwardsCompatible = visitor.getStaticContext.isInBackwardsCompatibleMode
    super.simplify(visitor)
  }

  /**
   * Type-check the expression
   * @return the checked expression
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.typeCheck(operand0, contextItemType)
    operand1 = visitor.typeCheck(operand1, contextItemType)
    val env = visitor.getStaticContext
    var comp = env.getConfiguration.getNamedCollation(env.getDefaultCollationName)
    if (comp == null) {
      comp = CodepointCollator.getInstance
    }
    comparer = new GenericAtomicComparer(comp, env.getConfiguration.getImplicitTimezone)
    this
  }

  def setAtomicComparer(comparer: AtomicComparer) {
    this.comparer = comparer
  }

  /**
   * Optimize the expression
   * @return the checked expression
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val config = visitor.getConfiguration
    operand0 = visitor.optimize(operand0, contextItemType)
    operand1 = visitor.optimize(operand1, contextItemType)
    operand0 = ExpressionTool.unsorted(config, operand0, false)
    operand1 = ExpressionTool.unsorted(config, operand1, false)
    this
  }

  /**
   * Evaluate the expression in a given context
   * @param context the given context for evaluation
   * @return a BooleanValue representing the result of the numeric comparison of the two operands
   */
  def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }

  /**
   * Evaluate the expression in a boolean context
   * @param context the given context for evaluation
   * @return a boolean representing the result of the numeric comparison of the two operands
   */
  def effectiveBooleanValue(context: XPathContext): Boolean = {
    var v0 = SequenceExtent.makeSequenceExtent(operand0.iterate(context))
    var v1 = SequenceExtent.makeSequenceExtent(operand1.iterate(context))
    if (backwardsCompatible) {
      if (v0.isInstanceOf[BooleanValue]) {
        v1 = BooleanValue.get(ExpressionTool.effectiveBooleanValue(v1.iterate()))
      } else if (v1.isInstanceOf[BooleanValue]) {
        v0 = BooleanValue.get(ExpressionTool.effectiveBooleanValue(v0.iterate()))
      }
    }
    var s0 = atomize(v0)
    var s1 = atomize(v1)
    if (backwardsCompatible) {
      if (operator == Token.LT || operator == Token.LE || operator == Token.GT || 
        operator == Token.GE) {
        val map = new ItemMappingFunction() {

          def mapItem(item: Item): Item = {
            return NumberFn.convert(item.asInstanceOf[AtomicValue])
          }
        }
        if (!(v0.isInstanceOf[DoubleValue])) {
          s0 = new ItemMappingIterator(s0, map, true)
        }
        if (!(v1.isInstanceOf[DoubleValue])) {
          s1 = new ItemMappingIterator(s1, map, true)
        }
      }
    }
    val val1 = SequenceExtent.makeSequenceExtent(s1)
    val singletonOperator = getSingletonOperator(operator)
    while (true) {
      val a = s0.next().asInstanceOf[AtomicValue]
      if (a == null) {
        return false
      }
      for (j <- 0 until val1.getLength) {
        val b = val1.itemAt(j).asInstanceOf[AtomicValue]
        if (compare(a, singletonOperator, b, comparer)) {
          true
        }
      }
    }
  }

  /**
   * Compare two atomic values
   *
   * @param a0 the first value to be compared
   * @param operator the comparison operator
   * @param a1 the second value to be compared
   * @param comparer the comparer to be used (perhaps including a collation)
   * @return the result of the comparison
   * @throws XPathException if comparison fails
   */
  private def compare(a0: AtomicValue, 
      operator: Int, 
      a1: AtomicValue, 
      comparer: AtomicComparer): Boolean = {
    val t0 = a0.getItemType
    val t1 = a1.getItemType
    if (backwardsCompatible) {
      if (a0.isInstanceOf[NumericValue] || a1.isInstanceOf[NumericValue]) {
        val v0 = NumberFn.convert(a0)
        val v1 = NumberFn.convert(a1)
        return ValueComparison.compare(v0, operator, v1, comparer, false)
      }
      if (t0 == AtomicType.STRING || t1 == AtomicType.STRING || 
        (t0 == AtomicType.UNTYPED_ATOMIC && t1 == AtomicType.UNTYPED_ATOMIC)) {
        val s0 = a0.convert(AtomicType.STRING).asAtomic().asInstanceOf[StringValue]
        val s1 = a1.convert(AtomicType.STRING).asAtomic().asInstanceOf[StringValue]
        return ValueComparison.compare(s0, operator, s1, comparer, false)
      }
    }
    if (t0 == AtomicType.UNTYPED_ATOMIC) {
      a0 = a0.convert(t1).asAtomic()
    }
    if (t1 == AtomicType.UNTYPED_ATOMIC) {
      a1 = a1.convert(t0).asAtomic()
    }
    ValueComparison.compare(a0, operator, a1, comparer, false)
  }

  /**
   * Determine the data type of the expression
   * @return Type.BOOLEAN
   */
  def getItemType(): ItemType = AtomicType.BOOLEAN
}

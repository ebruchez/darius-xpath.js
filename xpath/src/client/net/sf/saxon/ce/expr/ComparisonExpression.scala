package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.expr.sort.AtomicComparer
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface implemented by expressions that perform a comparison
 */
trait ComparisonExpression {

  /**
   * Get the AtomicComparer used to compare atomic values. This encapsulates any collation that is used
   */
  def getAtomicComparer(): AtomicComparer

  /**
   * Get the primitive (singleton) operator used: one of Token.FEQ, Token.FNE, Token.FLT, Token.FGT,
   * Token.FLE, Token.FGE
   */
  def getSingletonOperator(): Int

  /**
   * Get the two operands of the comparison
   */
  def getOperands(): Array[Expression]
}

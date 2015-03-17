package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.expr.sort.GlobalOrderComparer
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.BooleanValue
import client.net.sf.saxon.ce.value.SequenceType
import IdentityComparison._
//remove if not needed
import scala.collection.JavaConversions._

object IdentityComparison {

  private def getNode(exp: Expression, c: XPathContext): NodeInfo = {
    exp.evaluateItem(c).asInstanceOf[NodeInfo]
  }
}

/**
 * IdentityComparison: a boolean expression that compares two nodes
 * for equals, not-equals, greater-than or less-than based on identity and
 * document ordering
 */
class IdentityComparison(p1: Expression, op: Int, p2: Expression) extends BinaryExpression(p1, op, p2) {

  /**
   * Type-check the expression
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand0 = visitor.typeCheck(operand0, contextItemType)
    operand1 = visitor.typeCheck(operand1, contextItemType)
    val role0 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 0)
    operand0 = TypeChecker.staticTypeCheck(operand0, SequenceType.OPTIONAL_NODE, false, role0)
    val role1 = new RoleLocator(RoleLocator.BINARY_EXPR, Token.tokens(operator), 1)
    operand1 = TypeChecker.staticTypeCheck(operand1, SequenceType.OPTIONAL_NODE, false, role1)
    this
  }

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val node1 = getNode(operand0, context)
    if (node1 == null) {
      return null
    }
    val node2 = getNode(operand1, context)
    if (node2 == null) {
      return null
    }
    BooleanValue.get(compareIdentity(node1, node2))
  }

  def effectiveBooleanValue(context: XPathContext): Boolean = {
    val node1 = getNode(operand0, context)
    if (node1 == null) {
      return false
    }
    val node2 = getNode(operand1, context)
    node2 != null && compareIdentity(node1, node2)
  }

  private def compareIdentity(node1: NodeInfo, node2: NodeInfo): Boolean = operator match {
    case Token.IS => node1.isSameNodeInfo(node2)
    case Token.PRECEDES => GlobalOrderComparer.getInstance.compare(node1, node2) < 
      0
    case Token.FOLLOWS => GlobalOrderComparer.getInstance.compare(node1, node2) > 
      0
    case _ => throw new UnsupportedOperationException("Unknown node identity test")
  }

  /**
   * Determine the data type of the expression
   * @return Type.BOOLEAN
   */
  def getItemType(): ItemType = AtomicType.BOOLEAN
}

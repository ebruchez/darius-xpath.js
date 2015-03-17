package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import java.util.List
import java.util.Arrays
import WithParam._
//remove if not needed
import scala.collection.JavaConversions._

object WithParam {

  def simplify(params: Array[WithParam], visitor: ExpressionVisitor) {
    for (i <- 0 until params.length) {
      val select = params(i).getSelectExpression
      if (select != null) {
        params(i).setSelectExpression(visitor.simplify(select))
      }
    }
  }

  def typeCheck(params: Array[WithParam], visitor: ExpressionVisitor, contextItemType: ItemType) {
    for (i <- 0 until params.length) {
      val select = params(i).getSelectExpression
      if (select != null) {
        params(i).setSelectExpression(visitor.typeCheck(select, contextItemType))
      }
    }
  }

  def optimize(visitor: ExpressionVisitor, params: Array[WithParam], contextItemType: ItemType) {
    for (i <- 0 until params.length) {
      visitor.optimize(params(i), contextItemType)
    }
  }

  /**
   * Promote the expressions in a set of with-param elements. This is a convenience
   * method for use by containing instructions.
   */
  def promoteParams(parent: Expression, params: Array[WithParam], offer: PromotionOffer) {
    for (i <- 0 until params.length) {
      val select = params(i).getSelectExpression
      if (select != null) {
        params(i).setSelectExpression(select.promote(offer, parent))
      }
    }
  }

  /**
   * Get the XPath expressions used in an array of WithParam parameters (add them to the supplied list)
   */
  def getXPathExpressions(params: Array[WithParam], list: List[_]) {
    if (params != null) {
      list.addAll(Arrays.asList(params:_*))
    }
  }
}

/**
 * An instruction derived from a xsl:with-param element in the stylesheet. <br>
 */
class WithParam extends GeneralVariable {

  var parameterId: Int = _

  var typeChecked: Boolean = false

  /**
   * Allocate a number which is essentially an alias for the parameter name,
   * unique within a stylesheet
   *
   * @param id the parameter id
   */
  def setParameterId(id: Int) {
    parameterId = id
  }

  /**
   * Say whether this parameter will have been typechecked by the caller to ensure it satisfies
   * the required type, in which case the callee need not do a dynamic type check
   *
   * @param checked true if the caller has done static type checking against the required type
   */
  def setTypeChecked(checked: Boolean) {
    typeChecked = checked
  }

  /**
   * Get the parameter id, which is essentially an alias for the parameter name,
   * unique within a stylesheet
   *
   * @return the parameter id
   */
  def getParameterId(): Int = parameterId

  def processLeavingTail(context: XPathContext): TailCall = null

  /**
   * Evaluate the variable (method exists only to satisfy the interface)
   */
  def evaluateVariable(context: XPathContext): Sequence = {
    throw new UnsupportedOperationException()
  }

  /**
   * Ask whether static type checking has been done
   *
   * @return true if the caller has done static type checking against the type required by the callee
   */
  def isTypeChecked(): Boolean = typeChecked
}

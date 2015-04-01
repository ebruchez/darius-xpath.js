// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import java.util.Iterator

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.GeneralVariable._
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator, StructuredQName}
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.value.SequenceType

object GeneralVariable {

  private val REQUIRED = 4
  private val TUNNEL = 8
  private val IMPLICITLY_REQUIRED = 16
}

/**
 * This class defines common behaviour across xsl:variable, xsl:param, and xsl:with-param;
 */
abstract class GeneralVariable extends Instruction with Binding {

  private var properties: Byte = 0

  var select: Expression = null

  protected var variableQName: StructuredQName = _

  var requiredType: SequenceType = _

  protected var slotNumber: Int = _

  protected var evaluationMode: Int = ExpressionTool.UNDECIDED

  /**
   * Initialize the properties of the variable
   * @param select the expression to which the variable is bound
   * @param qName the name of the variable
   */
  def init(select: Expression, qName: StructuredQName): Unit = {
    this.select = select
    variableQName = qName
    adoptChildExpression(select)
  }

  /**
   * Set the expression to which this variable is bound
   * @param select the initializing expression
   */
  def setSelectExpression(select: Expression): Unit = {
    this.select = select
    evaluationMode = ExpressionTool.UNDECIDED
    adoptChildExpression(select)
  }

  /**
   * Get the expression to which this variable is bound
   * @return the initializing expression
   */
  def getSelectExpression(): Expression = select

  /**
   * Set the required type of this variable
   * @param required the required type
   */
  def setRequiredType(required: SequenceType): Unit = {
    requiredType = required
  }

  /**
   * Get the required type of this variable
   * @return the required type
   */
  def getRequiredType: SequenceType = requiredType

  /**
   * Indicate that this variable represents a required parameter
   * @param requiredParam true if this is a required parameter
   */
  def setRequiredParam(requiredParam: Boolean): Unit = {
    if (requiredParam) {
      properties |= REQUIRED
    } else {
      properties &= ~REQUIRED
    }
  }

  /**
   * Indicate that this variable represents a parameter that is implicitly required (because there is no
   * usable default value)
   * @param requiredParam true if this is an implicitly required parameter
   */
  def setImplicitlyRequiredParam(requiredParam: Boolean): Unit = {
    if (requiredParam) {
      properties |= IMPLICITLY_REQUIRED
    } else {
      properties &= ~IMPLICITLY_REQUIRED
    }
  }

  /**
   * Indicate whether this variable represents a tunnel parameter
   * @param tunnel true if this is a tunnel parameter
   */
  def setTunnel(tunnel: Boolean): Unit = {
    if (tunnel) {
      properties |= TUNNEL
    } else {
      properties &= ~TUNNEL
    }
  }

  /**
   * Get the type of the result of this instruction. An xsl:variable instruction returns nothing, so the
   * type is empty.
   * @return the empty type.
   */
  override def getItemType: ItemType = EmptySequenceTest.getInstance

  /**
   * Get the cardinality of the result of this instruction. An xsl:variable instruction returns nothing, so the
   * type is empty.
   * @return the empty cardinality.
   */
  override def getCardinality: Int = StaticProperty.EMPTY

  def isGlobal: Boolean = false

  /**
   * If this is a local variable held on the local stack frame, return the corresponding slot number.
   * In other cases, return -1.
   */
  def getLocalSlotNumber: Int = slotNumber

  /**
   * Ask whether this variable represents a required parameter
   * @return true if this is a required parameter
   */
  def isRequiredParam(): Boolean = (properties & REQUIRED) != 0

  /**
   * Ask whether this variable represents a parameter that is implicitly required, because there is no usable
   * default value
   * @return true if this variable is an implicitly required parameter
   */
  def isImplicitlyRequiredParam(): Boolean = (properties & IMPLICITLY_REQUIRED) != 0

  /**
   * Ask whether this variable represents a tunnel parameter
   * @return true if this is a tunnel parameter
   */
  def isTunnelParam(): Boolean = (properties & TUNNEL) != 0

  /**
   * Simplify this expression
   * @param visitor an expression
   * @return the simplified expression
   * @throws XPathException
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    if (select != null) {
      select = visitor.simplify(select)
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (select != null) {
      select = visitor.typeCheck(select, contextItemType)
      adoptChildExpression(select)
    }
    checkAgainstRequiredType(visitor)
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (select != null) {
      select = visitor.optimize(select, contextItemType)
      adoptChildExpression(select)
      evaluationMode = computeEvaluationMode()
    }
    this
  }

  private def computeEvaluationMode(): Int = {
    ExpressionTool.lazyEvaluationMode(select)
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  def copy(): Expression = {
    throw new UnsupportedOperationException("GeneralVariable.copy()")
  }

  /**
   * Check the select expression against the required type.
   * @param visitor an expression visitor
   * @throws XPathException
   */
  private def checkAgainstRequiredType(visitor: ExpressionVisitor): Unit = {
    val role = new RoleLocator(RoleLocator.VARIABLE, variableQName, 0)
    val r = requiredType
    if (r != null && select != null) {
      select = TypeChecker.staticTypeCheck(select, requiredType, backwardsCompatible = false, role)
    }
  }

  /**
   * Evaluate an expression as a single item. This always returns either a single Item or
   * null (denoting the empty sequence). No conversion is done. This method should not be
   * used unless the static type of the expression is a subtype of "item" or "item?": that is,
   * it should not be called if the expression may return a sequence. There is no guarantee that
   * this condition will be detected.
   *
   * @param context The context in which the expression is to be evaluated
   * @throws XPathException if any dynamic error occurs evaluating the
   *     expression
   * @return the node or atomic value that results from evaluating the
   *     expression; or null to indicate that the result is an empty
   *     sequence
   */
  override def evaluateItem(context: XPathContext): Item = {
    process(context)
    null
  }

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation relies on the process() method: it
   * "pushes" the results of the instruction to a sequence in memory, and then
   * iterates over this in-memory sequence.
   *
   * In principle instructions should implement a pipelined iterate() method that
   * avoids the overhead of intermediate storage.
   *
   * @throws XPathException if any dynamic error occurs evaluating the
   *     expression
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *     of the expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    evaluateItem(context)
    EmptyIterator.getInstance
  }

  /**
   * Evaluate the variable. That is,
   * get the value of the select expression if present or the content
   * of the element otherwise, either as a tree or as a sequence
   * @param context the XPath dynamic context
   * @return the result of evaluating the variable
   */
  def getSelectValue(context: XPathContext): Sequence = {
    if (select == null) {
      throw new AssertionError("*** No select expression!!")
    } else {
      ExpressionTool.evaluate(select, evaluationMode, context)
    }
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  override protected def promoteInst(offer: PromotionOffer): Unit = {
    if (select != null) {
      val e2 = doPromotion(select, offer)
      if (e2 != select) {
        select = e2
        evaluationMode = computeEvaluationMode()
      }
    }
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  override def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select)

  /**
   * Get the slot number allocated to this variable
   * @return the slot number, that is the position allocated to the variable on its stack frame
   */
  def getSlotNumber(): Int = slotNumber

  /**
   * Set the slot number of this variable
   * @param s the slot number, that is, the position allocated to this variable on its stack frame
   */
  def setSlotNumber(s: Int): Unit = {
    slotNumber = s
  }

  /**
   * Set the name of the variable
   * @param s the name of the variable (a QName)
   */
  def setVariableQName(s: StructuredQName): Unit = {
    variableQName = s
  }

  /**
   * Get the name of this variable
   * @return the name of this variable (a QName)
   */
  def getVariableQName: StructuredQName = variableQName
}

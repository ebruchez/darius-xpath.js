// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.NameChecker
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.Whitespace
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:processing-instruction element in the stylesheet, or a processing-instruction
 * constructor in a query
 */
class ProcessingInstruction(var name: Expression) extends SimpleNodeConstructor {

  adoptChildExpression(name)

  /**
   * Get the expression that defines the processing instruction name
   * @return the expression that defines the processing instruction name
   */
  def getNameExpression(): Expression = name

  def getItemType(): ItemType = NodeKindTest.PROCESSING_INSTRUCTION

  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def simplify(visitor: ExpressionVisitor): Expression = {
    name = visitor.simplify(name)
    super.simplify(visitor)
  }

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType) {
    name = visitor.typeCheck(name, contextItemType)
    adoptChildExpression(name)
    val role = new RoleLocator(RoleLocator.INSTRUCTION, "processing-instruction/name", 0)
    name = TypeChecker.staticTypeCheck(name, SequenceType.SINGLE_STRING, false, role)
    adoptChildExpression(name)
  }

  def getDependencies(): Int = {
    name.getDependencies | super.getDependencies
  }

  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select, name)

  /**
   * Offer promotion for subexpressions. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *     expressions that don't depend on the context to an outer level in
   *     the containing expression
   * @exception XPathException if any error is detected
   */
  protected def promoteInst(offer: PromotionOffer) {
    name = doPromotion(name, offer)
    super.promoteInst(offer)
  }

  /**
   * Process the value of the node, to create the new node.
   * @param value the string value of the new node
   * @param context the dynamic evaluation context
   * @throws XPathException
   */
  def processValue(value: CharSequence, context: XPathContext) {
    val expandedName = evaluateName(context)
    if (expandedName != null) {
      val data = checkContent(value.toString, context)
      val out = context.getReceiver
      out.processingInstruction(expandedName, data)
    }
  }

  /**
   * Check the content of the node, and adjust it if necessary
   *
   * @param data the supplied content
   * @return the original content, unless adjustments are needed
   * @throws XPathException if the content is invalid
   */
  protected def checkContent(data: String, context: XPathContext): String = {
    var hh: Int = 0
    while ((hh = data.indexOf("?>")) >= 0) {
      data = data.substring(0, hh + 1) + ' ' + data.substring(hh + 1)
    }
    data = Whitespace.removeLeadingWhitespace(data).toString
    data
  }

  def evaluateNameCode(context: XPathContext): StructuredQName = {
    val expandedName = evaluateName(context)
    new StructuredQName("", "", expandedName)
  }

  /**
   * Evaluate the name of the processing instruction.
   * @param context
   * @return the name of the processing instruction (an NCName), or null, incicating an invalid name
   * @throws XPathException if evaluation fails, or if the recoverable error is treated as fatal
   */
  private def evaluateName(context: XPathContext): String = {
    var expandedName: String = null
    try {
      expandedName = Whitespace.trim(name.evaluateAsString(context))
    } catch {
      case err: ClassCastException => dynamicError("Processing instruction name is not a string", "XQDY0041")
    }
    checkName(expandedName, context)
    expandedName
  }

  private def checkName(expandedName: String, context: XPathContext) {
    if (!(NameChecker.isValidNCName(expandedName))) {
      dynamicError("Processing instruction name " + Err.wrap(expandedName) + 
        " is not a valid NCName", "XTDE0890")
    }
    if (expandedName.equalsIgnoreCase("xml")) {
      dynamicError("Processing instructions cannot be named 'xml' in any combination of upper/lower case", 
        "XTDE0890")
    }
  }
}

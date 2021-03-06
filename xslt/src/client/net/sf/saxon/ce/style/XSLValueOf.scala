// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.LogController
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.ValueOf
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.AtomicType
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.`type`.TypeHierarchy
import org.orbeon.darius.xpath.value.Cardinality
import org.orbeon.darius.xpath.value.StringValue
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:value-of element in the stylesheet. <br>
 * The xsl:value-of element takes attributes:<ul>
 * <li>a mandatory attribute select="expression".
 * This must be a valid String expression</li>
 * <li>an optional disable-output-escaping attribute, value "yes" or "no"</li>
 * <li>an optional separator attribute</li>
 * </ul>
 */
class XSLValueOf extends XSLLeafNodeConstructor {

  private var separator: Expression = _

  private var selectAttTrace: String = ""

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = NodeKindTest.TEXT

  def prepareAttributes(): Unit = {
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    separator = checkAttribute("separator", "a").asInstanceOf[Expression]
    checkAttribute("disable-output-escaping", "b")
    checkForUnknownAttributes()
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      selectAttTrace = getAttributeValue("", "select")
    }
  }

  def validate(decl: Declaration): Unit = {
    super.validate(decl)
    select = typeCheck(select)
    separator = typeCheck(separator)
  }

  /**
   * Get the error code to be returned when the element has a select attribute but is not empty.
   *
   * @return the error code defined for this condition, for this particular instruction
   */
  protected def getErrorCodeForSelectPlusContent(): String = "XTSE0870"

  def compile(exec: Executable, decl: Declaration): Expression = {
    val th = TypeHierarchy.getInstance
    if (separator == null && select != null && xPath10ModeIsEnabled()) {
      if (!select.getItemType.isInstanceOf[AtomicType]) {
        select = new Atomizer(select)
        select = makeExpressionVisitor().simplify(select)
      }
      if (Cardinality.allowsMany(select.getCardinality)) {
        select = new FirstItemExpression(select)
      }
      if (!th.isSubType(select.getItemType, AtomicType.STRING)) {
        select = new AtomicSequenceConverter(select, AtomicType.STRING)
      }
    } else {
      if (separator == null) {
        separator = if (select == null) new StringLiteral(StringValue.EMPTY_STRING) else new StringLiteral(StringValue.SINGLE_SPACE)
      }
    }
    val inst = new ValueOf(select, false)
    compileContent(exec, decl, inst, separator)
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      inst.AddTraceProperty("select", selectAttTrace)
    }
    inst
  }
}

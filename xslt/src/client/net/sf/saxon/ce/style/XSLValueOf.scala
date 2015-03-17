package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.ValueOf
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.Cardinality
import client.net.sf.saxon.ce.value.StringValue
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

  def prepareAttributes() {
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    separator = checkAttribute("separator", "a").asInstanceOf[Expression]
    checkAttribute("disable-output-escaping", "b")
    checkForUnknownAttributes()
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      selectAttTrace = getAttributeValue("", "select")
    }
  }

  def validate(decl: Declaration) {
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
      if (!(select.getItemType.isInstanceOf[AtomicType])) {
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

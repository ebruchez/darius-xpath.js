package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionTool
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import java.util.Iterator
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The compiled form of an xsl:param element within a template in an XSLT stylesheet.
 *
 * <p>The xsl:param element in XSLT has mandatory attribute name and optional attribute select. It can also
 * be specified as required="yes" or required="no".</p>
 *
 * <p>This is used only for parameters to XSLT templates. For function calls, the caller of the function
 * places supplied arguments onto the callee's stackframe and the callee does not need to do anything.
 * Global parameters (XQuery external variables) are handled using {@link GlobalParam}.</p>
 *
 */
class LocalParam extends GeneralVariable {

  @BeanProperty
  var parameterId: Int = _

  private var conversion: Expression = null

  private var conversionEvaluationMode: Int = ExpressionTool.UNDECIDED

  /**
   * Define a conversion that is to be applied to the supplied parameter value.
   * @param convertor The expression to be applied. This performs type checking,
   * and the basic conversions implied by function calling rules, for example
   * numeric promotion, atomization, and conversion of untyped atomic values to
   * a required type. The conversion uses the actual parameter value as input,
   * referencing it using a VariableReference.
   */
  def setConversion(convertor: Expression) {
    conversion = convertor
    if (convertor != null) {
      conversionEvaluationMode = ExpressionTool.eagerEvaluationMode(conversion)
    }
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select, conversion)

  /**
   * Process the local parameter declaration
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    val params = (if (isTunnelParam) context.getTunnelParameters else context.getLocalParameters)
    val index = (if (params == null) -1 else params.getIndex(getParameterId))
    if (index < 0) {
      if (isImplicitlyRequiredParam) {
        val name = "$" + getVariableQName.getDisplayName
        throw new XPathException("A value must be supplied for parameter " + name + " because " + 
          "the default value is not a valid instance of the required type", "XTDE0610")
      } else if (isRequiredParam) {
        val name = "$" + getVariableQName.getDisplayName
        throw new XPathException("No value supplied for required parameter " + name, "XTDE0700")
      }
      context.setLocalVariable(getSlotNumber, getSelectValue(context))
    } else {
      assert(params != null)
      val `val` = params.getValue(index)
      context.setLocalVariable(getSlotNumber, `val`)
      val checked = params.isTypeChecked(index)
      if (!checked && conversion != null) {
        context.setLocalVariable(getSlotNumber, ExpressionTool.evaluate(conversion, conversionEvaluationMode, 
          context))
      }
    }
    null
  }

  /**
   * Evaluate the variable
   */
  def evaluateVariable(c: XPathContext): Sequence = c.evaluateLocalVariable(slotNumber)
}

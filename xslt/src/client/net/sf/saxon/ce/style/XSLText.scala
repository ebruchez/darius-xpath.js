package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.ValueOf
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:text elements in stylesheet. <BR>
 */
class XSLText extends XSLLeafNodeConstructor {

  private var value: StringValue = _

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = NodeKindTest.TEXT

  def prepareAttributes() {
    checkAttribute("disable-output-escaping", "b")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    value = StringValue.EMPTY_STRING
    for (child <- allChildren()) {
      if (child.isInstanceOf[StyleElement]) {
        child.asInstanceOf[StyleElement].compileError("xsl:text must not contain child elements", "XTSE0010")
        return
      } else {
        value = StringValue.makeStringValue(child.getStringValue)
      }
    }
    super.validate(decl)
  }

  /**
   * Get the error code to be returned when the element has a select attribute but is not empty.
   * @return the error code defined for this condition, for this particular instruction
   */
  protected def getErrorCodeForSelectPlusContent(): String = null

  def compile(exec: Executable, decl: Declaration): Expression = {
    new ValueOf(Literal.makeLiteral(value), false)
  }
}

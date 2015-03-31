// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.StringLiteral
import client.net.sf.saxon.ce.expr.instruct._
import client.net.sf.saxon.ce.om.InscopeNamespaceResolver
import client.net.sf.saxon.ce.om.NamespaceResolver
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.StringValue
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * xsl:attribute element in stylesheet. <br>
 */
class XSLAttribute extends XSLLeafNodeConstructor {

  private var attributeName: Expression = _

  private var separator: Expression = _

  private var namespace: Expression = _

  def prepareAttributes(): Unit = {
    attributeName = checkAttribute("name", "a1").asInstanceOf[Expression]
    namespace = checkAttribute("namespace", "a").asInstanceOf[Expression]
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    separator = checkAttribute("separator", "a").asInstanceOf[Expression]
    checkAttribute("validation", "v")
    checkAttribute("type", "t")
    checkForUnknownAttributes()
    if (separator == null) {
      separator = if (select == null) new StringLiteral(StringValue.EMPTY_STRING) else new StringLiteral(StringValue.SINGLE_SPACE)
    }
  }

  def validate(decl: Declaration): Unit = {
    attributeName = typeCheck(attributeName)
    namespace = typeCheck(namespace)
    select = typeCheck(select)
    separator = typeCheck(separator)
    super.validate(decl)
  }

  /**
   * Get the error code to be returned when the element has a select attribute but is not empty.
   *
   * @return the error code defined for this condition, for this particular instruction
   */
  protected def getErrorCodeForSelectPlusContent(): String = "XTSE0840"

  def compile(exec: Executable, decl: Declaration): Expression = {
    var inst: SimpleNodeConstructor = null
    if (attributeName.isInstanceOf[StringLiteral] && namespace == null && 
      attributeName.asInstanceOf[StringLiteral].getStringValue
      .indexOf(':') < 
      0) {
      val localName = Whitespace.trim(attributeName.asInstanceOf[StringLiteral].getStringValue)
      inst = new FixedAttribute(new StructuredQName("", "", localName))
    } else {
      val resolver = new InscopeNamespaceResolver(this)
      inst = new ComputedAttribute(attributeName, namespace, resolver)
    }
    inst.setContainer(this)
    compileContent(exec, decl, inst, separator)
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      inst.AddTraceProperty("name", attributeName)
    }
    inst
  }
}

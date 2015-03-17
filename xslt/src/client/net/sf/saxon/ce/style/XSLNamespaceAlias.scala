// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.om.InscopeNamespaceResolver
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.NamespaceResolver
import client.net.sf.saxon.ce.trans.XPathException
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:namespace-alias element in the stylesheet. <br>
 */
class XSLNamespaceAlias extends StyleElement {

  @BeanProperty
  var stylesheetURI: String = _

  @BeanProperty
  var resultNamespaceBinding: NamespaceBinding = _

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes() {
    var stylesheetPrefix = checkAttribute("stylesheet-prefix", "w1").asInstanceOf[String]
    var resultPrefix = checkAttribute("result-prefix", "w1").asInstanceOf[String]
    checkForUnknownAttributes()
    if (stylesheetPrefix == "#default") {
      stylesheetPrefix = ""
    }
    if (resultPrefix == "#default") {
      resultPrefix = ""
    }
    val resolver = new InscopeNamespaceResolver(this)
    stylesheetURI = resolver.getURIForPrefix(stylesheetPrefix, true)
    if (stylesheetURI == null) {
      compileError("stylesheet-prefix " + stylesheetPrefix + " has not been declared", "XTSE0812")
      stylesheetURI = ""
      resultNamespaceBinding = NamespaceBinding.DEFAULT_UNDECLARATION
      return
    }
    var resultURI = resolver.getURIForPrefix(resultPrefix, true)
    if (resultURI == null) {
      compileError("result-prefix " + resultPrefix + " has not been declared", "XTSE0812")
      stylesheetURI = ""
      resultURI = ""
    }
    resultNamespaceBinding = new NamespaceBinding(resultPrefix, resultURI)
  }

  def validate(decl: Declaration) {
    checkTopLevel(null)
  }

  def compile(exec: Executable, decl: Declaration): Expression = null

  protected def index(decl: Declaration, top: PrincipalStylesheetModule) {
    top.addNamespaceAlias(decl)
  }
}

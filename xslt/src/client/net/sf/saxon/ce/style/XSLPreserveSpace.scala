// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.Template
import client.net.sf.saxon.ce.om.InscopeNamespaceResolver
import client.net.sf.saxon.ce.om.NamespaceResolver
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern._
import client.net.sf.saxon.ce.trans.StripSpaceRules
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.Whitespace
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:preserve-space or xsl:strip-space elements in stylesheet. <br>
 */
class XSLPreserveSpace extends StyleElement {

  private var elements: String = "*"

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes(): Unit = {
    elements = checkAttribute("elements", "s1").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    checkEmpty()
    checkTopLevel(null)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val preserve = if (getLocalPart == "preserve-space") StripSpaceRules.PRESERVE else StripSpaceRules.STRIP
    val stripperRules = getExecutable.getStripperRules
    val resolver = new InscopeNamespaceResolver(this)
    for (s ← Whitespace.tokenize(elements)) {
      var nt: NodeTest = null
      if (s == "*") {
        nt = NodeKindTest.ELEMENT
        stripperRules.addRule(nt, preserve, decl.getModule)
      } else if (s.endsWith(":*")) {
        if (s.length == 2) {
          compileError("No prefix before ':*'")
        }
        val prefix = s.substring(0, s.length - 2)
        val uri = resolver.getURIForPrefix(prefix, useDefault = false)
        nt = new NamespaceTest(Type.ELEMENT, uri)
        stripperRules.addRule(nt, preserve, decl.getModule)
      } else if (s.startsWith("*:")) {
        if (s.length == 2) {
          compileError("No local name after '*:'")
        }
        val localname = s.substring(2)
        nt = new LocalNameTest(Type.ELEMENT, localname)
        stripperRules.addRule(nt, preserve, decl.getModule)
      } else {
        var qn: StructuredQName = null
        try {
          qn = StructuredQName.fromLexicalQName(s, getDefaultXPathNamespace, resolver)
        } catch {
          case err: XPathException ⇒ {
            compileError("Element name " + s + " is not a valid QName", "XTSE0280")
            return null
          }
        }
        nt = new NameTest(Type.ELEMENT, qn)
        stripperRules.addRule(nt, preserve, decl.getModule)
      }
    }
    null
  }
}

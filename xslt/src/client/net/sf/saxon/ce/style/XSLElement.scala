// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.LogController
import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.Literal
import org.orbeon.darius.xpath.expr.StringLiteral
import org.orbeon.darius.xpath.expr.instruct._
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.EmptySequence
import com.google.gwt.logging.client.LogConfiguration
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:element element in the stylesheet. <br>
 */
class XSLElement extends StyleElement {

  private var elementName: Expression = _

  private var namespace: Expression = null

  private var use: String = _

  private var attributeSets: Array[AttributeSet] = null

  private var inheritNamespaces: Boolean = true

  /**
   * Determine whether this node is an instruction.
   *
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   *
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes(): Unit = {
    elementName = checkAttribute("name", "a1").asInstanceOf[Expression]
    namespace = checkAttribute("namespace", "a").asInstanceOf[Expression]
    checkAttribute("validation", "v")
    checkAttribute("type", "t")
    val b = checkAttribute("inherit-namespaces", "b").asInstanceOf[java.lang.Boolean]
    if (b != null) {
      inheritNamespaces = b
    }
    use = checkAttribute("use-attribute-sets", "s").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    if (use != null) {
      attributeSets = getAttributeSets(use, null)
    }
    elementName = typeCheck(elementName)
    namespace = typeCheck(namespace)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val resolver = new InscopeNamespaceResolver(this)
    if (elementName.isInstanceOf[StringLiteral]) {
      val qName = elementName.asInstanceOf[StringLiteral].getStringValue
      var parts: Array[String] = null
      try {
        parts = NameChecker.getQNameParts(qName)
      } catch {
        case e: QNameException ⇒
          compileError("Invalid element name: " + qName, "XTDE0820")
          return null
      }
      var nsuri: String = null
      if (namespace.isInstanceOf[StringLiteral]) {
        nsuri = namespace.asInstanceOf[StringLiteral].getStringValue
        if (nsuri.length == 0) {
          parts(0) = ""
        }
      } else if (namespace == null) {
        nsuri = resolver.getURIForPrefix(parts(0), useDefault = true)
        if (nsuri == null) {
          undeclaredNamespaceError(parts(0), "XTDE0830")
        }
      }
      if (nsuri != null) {
        val nameCode = new StructuredQName(parts(0), nsuri, parts(1))
        val inst = new FixedElement(nameCode, null, inheritNamespaces)
        inst.setBaseURI(getBaseURI)
        if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
          inst.AddTraceProperty("name", elementName)
        }
        return compileContentExpression(exec, decl, inst)
      }
    }
    val inst = new ComputedElement(elementName, namespace, resolver, inheritNamespaces)
    compileContentExpression(exec, decl, inst)
  }

  private def compileContentExpression(exec: Executable, decl: Declaration, inst: ElementCreator): Expression = {
    var content = compileSequenceConstructor(exec, decl)
    if (attributeSets != null) {
      val use = new UseAttributeSets(attributeSets)
      if (content == null) {
        content = use
      } else {
        content = Block.makeBlock(use, content)
        content.setSourceLocator(this)
      }
    }
    if (content == null) {
      content = new Literal(EmptySequence.getInstance)
    }
    inst.setContentExpression(content)
    inst
  }
}

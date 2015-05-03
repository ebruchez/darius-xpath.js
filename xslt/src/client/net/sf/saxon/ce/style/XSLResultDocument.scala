// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.Literal
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.ResultDocument
import org.orbeon.darius.xpath.om.InscopeNamespaceResolver
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.value.EmptySequence
import java.util.ArrayList
import java.util.List
import XSLResultDocument._
//remove if not needed
import scala.collection.JavaConversions._

object XSLResultDocument {

  private val fans = new ArrayList[String](25)

  fans.add("method")

  fans.add("output-version")

  fans.add("byte-order-mark")

  fans.add("indent")

  fans.add("encoding")

  fans.add("media-type")

  fans.add("doctype-system")

  fans.add("doctype-public")

  fans.add("omit-xml-declaration")

  fans.add("standalone")

  fans.add("cdata-section-elements")

  fans.add("include-content-type")

  fans.add("escape-uri-attributes")

  fans.add("undeclare-prefixes")

  fans.add("normalization-form")

  fans.add("use-character-maps")
}

/**
 * An xsl:result-document element in the stylesheet. <BR>
 * The xsl:result-document element takes an attribute href="filename". The filename will
 * often contain parameters, e.g. {position()} to ensure that a different file is produced
 * for each element instance. <BR>
 * There is a further attribute "name" which determines the format of the
 * output file, it identifies the name of an xsl:output element containing the output
 * format details.
 */
class XSLResultDocument extends StyleElement {

  private var href: Expression = _

  private var formatQName: StructuredQName = _

  private var methodExpression: Expression = _

  private var method: Expression = _

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

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction). Default implementation returns Type.ITEM, indicating
   * that we don't know, it might be anything. Returns null in the case of an element
   * such as xsl:sort or xsl:variable that can appear in a sequence constructor but
   * contributes nothing to the result sequence.
   *
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = null

  def prepareAttributes(): Unit = {
    for (att ← fans) {
      checkAttribute(att, "s")
    }
    methodExpression = checkAttribute("method", "a").asInstanceOf[Expression]
    checkAttribute("validation", "v")
    checkAttribute("type", "t")
    checkAttribute("format", "q")
    href = checkAttribute("href", "a").asInstanceOf[Expression]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    href = typeCheck(href)
    methodExpression = typeCheck(methodExpression)
    getExecutable.setCreatesSecondaryResult(true)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val inst = new ResultDocument(href, methodExpression, getBaseURI, new InscopeNamespaceResolver(this))
    var b = compileSequenceConstructor(exec, decl)
    if (b == null) {
      b = new Literal(EmptySequence.getInstance)
    }
    inst.setContentExpression(b)
    inst
  }
}

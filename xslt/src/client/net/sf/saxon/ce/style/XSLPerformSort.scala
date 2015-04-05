// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.sort.SortExpression
import client.net.sf.saxon.ce.expr.sort.SortKeyDefinition
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:perform-sort elements in stylesheet (XSLT 2.0). <br>
 */
class XSLPerformSort extends StyleElement {

  var select: Expression = null

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = {
    if (select == null) {
      getCommonChildItemType
    } else {
      select.getItemType
    }
  }

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  /**
   * Specify that xsl:sort is a permitted child
   */
  protected def isPermittedChild(child: StyleElement): Boolean = child.isInstanceOf[XSLSort]

  def prepareAttributes(): Unit = {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    checkSortComesFirst(true)
    if (select != null) {
      onlyAllow("fallback", "sort")
    }
    select = typeCheck(select)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val sortKeys = makeSortKeys(decl)
    if (select != null) {
      new SortExpression(select, sortKeys)
    } else {
      var body = compileSequenceConstructor(exec, decl)
      if (body == null) {
        body = Literal.makeEmptySequence()
      }
      try {
        new SortExpression(makeExpressionVisitor().simplify(body), sortKeys)
      } catch {
        case e: XPathException ⇒
          compileError(e)
          null
      }
    }
  }
}

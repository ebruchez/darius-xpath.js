// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.ForEach
import client.net.sf.saxon.ce.expr.sort.SortExpression
import client.net.sf.saxon.ce.expr.sort.SortKeyDefinition
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.Cardinality
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:for-each elements in stylesheet. <br>
 */
class XSLForEach extends StyleElement {

  private var select: Expression = null

  private var containsTailCall: Boolean = false

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Specify that xsl:sort is a permitted child
   */
  protected def isPermittedChild(child: StyleElement): Boolean = (child.isInstanceOf[XSLSort])

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  protected def markTailCalls(): Boolean = {
    if (Cardinality.allowsMany(select.getCardinality)) {
      false
    } else {
      val last = getLastChildInstruction
      containsTailCall = last != null && last.markTailCalls()
      containsTailCall
    }
  }

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes() {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    checkSortComesFirst(false)
    select = typeCheck(select)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val sortKeys = makeSortKeys(decl)
    var sortedSequence = select
    if (sortKeys != null) {
      sortedSequence = new SortExpression(select, sortKeys)
    }
    val block = compileSequenceConstructor(exec, decl)
    if (block == null) {
      return Literal.makeEmptySequence()
    }
    try {
      new ForEach(sortedSequence, makeExpressionVisitor().simplify(block), containsTailCall)
    } catch {
      case err: XPathException => {
        compileError(err)
        null
      }
    }
  }
}

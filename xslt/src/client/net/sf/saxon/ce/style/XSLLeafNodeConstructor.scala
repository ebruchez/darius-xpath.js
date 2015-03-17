// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.SimpleNodeConstructor
import client.net.sf.saxon.ce.expr.instruct.ValueOf
import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.value.StringValue
import XSLLeafNodeConstructor._
//remove if not needed
import scala.collection.JavaConversions._

object XSLLeafNodeConstructor {

  /**
   * Construct an expression that implements the rules of "constructing simple content":
   * given an expression to select the base sequence, and an expression to compute the separator,
   * build an (unoptimized) expression to produce the value of the node as a string.
   * @param select the expression that selects the base sequence
   * @param separator the expression that computes the separator
   * @return an expression that returns a string containing the string value of the constructed node
   */
  def makeSimpleContentConstructor(select: Expression, separator: Expression): Expression = {
    if (select.isInstanceOf[ValueOf] && 
      select.asInstanceOf[ValueOf].getContentExpression.isInstanceOf[StringLiteral]) {
      return select.asInstanceOf[ValueOf].getContentExpression
    }
    select = new AdjacentTextNodeMerger(select)
    select = new Atomizer(select)
    select = new AtomicSequenceConverter(select, AtomicType.STRING)
    select = SystemFunction.makeSystemFunction("string-join", Array(select, separator))
    select
  }
}

/**
 * Common superclass for XSLT elements whose content template produces a text
 * value: xsl:text, xsl:value-of, xsl:attribute, xsl:comment, and xsl:processing-instruction
 */
abstract class XSLLeafNodeConstructor extends StyleElement {

  protected var select: Expression = null

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

  def validate(decl: Declaration) {
    if (select != null && hasChildNodes()) {
      val errorCode = getErrorCodeForSelectPlusContent
      compileError("An " + getDisplayName + " element with a select attribute must be empty", errorCode)
    }
  }

  /**
   * Get the error code to be returned when the element has a select attribute but is not empty.
   * @return the error code defined for this condition, for this particular instruction
   */
  protected def getErrorCodeForSelectPlusContent(): String

  protected def compileContent(exec: Executable, 
      decl: Declaration, 
      inst: SimpleNodeConstructor, 
      separator: Expression) {
    if (separator == null) {
      separator = new StringLiteral(StringValue.SINGLE_SPACE)
    }
    try {
      if (select == null) {
        select = compileSequenceConstructor(exec, decl)
      }
      select = makeSimpleContentConstructor(select, separator)
      inst.setSelect(select, exec.getConfiguration)
    } catch {
      case err: XPathException => compileError(err)
    }
  }
}

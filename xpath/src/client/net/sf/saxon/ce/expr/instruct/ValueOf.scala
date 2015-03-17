// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.Orphan
import client.net.sf.saxon.ce.value.StringValue


/**
 * An xsl:value-of element in the stylesheet. <br>
 * The xsl:value-of element takes attributes:<ul>
 * <li>a mandatory attribute select="expression".
 * This must be a valid String expression</li>
 * <li>an optional disable-output-escaping attribute, value "yes" or "no"</li>
 * <li>an optional separator attribute. This is handled at compile-time: if the separator attribute
 * is present, the select expression passed in here will be a call to the string-join() function.</li>
 * </ul>
 */
class ValueOf(select: Expression, var noNodeIfEmpty: Boolean) extends SimpleNodeConstructor {

  var isNumberingInstruction: Boolean = false

  setSelect(select, null)//ORBEON could write this way
//  super.select = select
//  adoptChildExpression(select)

  /**
   * Indicate that this is really an xsl:nunber instruction
   */
  def setIsNumberingInstruction() {
    isNumberingInstruction = true
  }

  override def getItemType(): ItemType = NodeKindTest.TEXT

  override def computeCardinality(): Int = {
    if (noNodeIfEmpty) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.EXACTLY_ONE
    }
  }

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType) {
  }

  /**
   * Convert this value-of instruction to an expression that delivers the string-value of the resulting
   * text node as an untyped atomic value.
   * @return the converted expression
   */
  def convertToCastAsString(): Expression = {
    new CastExpression(select, AtomicType.UNTYPED_ATOMIC, true)
  }

  /**
   * Process this instruction
   * @param context the dynamic context of the transformation
   * @return a TailCall to be executed by the caller, always null for this instruction
   */
  override def processLeavingTail(context: XPathContext): TailCall = {
    if (noNodeIfEmpty) {
      val value = select.evaluateItem(context).asInstanceOf[StringValue]
      if (value != null) {
        processValue(value.getStringValue, context)
      }
      null
    } else {
      super.processLeavingTail(context)
    }
  }

  /**
   * Process the value of the node, to create the new node.
   * @param value   the string value of the new node
   * @param context the dynamic evaluation context
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *
   */
  def processValue(value: CharSequence, context: XPathContext) {
    //ORBEON XSLT only called via processLeavingTail called via process which is never called in pure XPath
//    val out = context.getReceiver~
//    out.characters(value)
  }

  /**
   * Evaluate this expression, returning the resulting text node to the caller
   * @param context the dynamic evaluation context
   * @return the parentless text node that results from evaluating this instruction, or null to
   * represent an empty sequence
   * @throws XPathException
   */
  override def evaluateItem(context: XPathContext): Item = {
    try {
      var `val`: CharSequence = null
      val item = select.evaluateItem(context)
      if (item == null) {
        if (noNodeIfEmpty) {
          return null
        } else {
          `val` = ""
        }
      } else {
        `val` = item.getStringValue
      }
      val o = new Orphan()
      o.setNodeKind(Type.TEXT)
      o.setStringValue(`val`)
      o
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
  }
}

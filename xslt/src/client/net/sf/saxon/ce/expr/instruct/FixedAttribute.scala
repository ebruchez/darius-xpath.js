// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction derived from an xsl:attribute element in stylesheet, or from
 * an attribute constructor in XQuery. This version deals only with attributes
 * whose name is known at compile time. It is also used for attributes of
 * literal result elements. The value of the attribute is in general computed
 * at run-time.
 */
class FixedAttribute(var nameCode: StructuredQName) extends AttributeCreator {

  /**
   * Set the expression defining the value of the attribute. If this is a constant, and if
   * validation against a schema type was requested, the validation is done immediately.
   * @param select The expression defining the content of the attribute
   * @param config The Saxon configuration
   * @throws XPathException if the expression is a constant, and validation is requested, and
   * the constant doesn't match the required type.
   */
  def setSelect(select: Expression, config: Configuration) {
    super.setSelect(select, config)
    if (nameCode == StructuredQName.XML_ID) {
      select = SystemFunction.makeSystemFunction("normalize-space", Array(select))
      super.setSelect(select, config)
    }
  }

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType) {
  }

  def getItemType(): ItemType = NodeKindTest.ATTRIBUTE

  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def evaluateNameCode(context: XPathContext): StructuredQName = nameCode
}

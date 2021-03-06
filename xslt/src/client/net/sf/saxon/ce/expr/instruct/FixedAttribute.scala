// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.ExpressionVisitor
import org.orbeon.darius.xpath.expr.StaticProperty
import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.functions.SystemFunction
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.ItemType
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
  def setSelect(select: Expression, config: Configuration): Unit = {
    super.setSelect(select, config)
    if (nameCode == StructuredQName.XML_ID) {
      select = SystemFunction.makeSystemFunction("normalize-space", Array(select))
      super.setSelect(select, config)
    }
  }

  def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Unit = {
  }

  def getItemType(): ItemType = NodeKindTest.ATTRIBUTE

  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def evaluateNameCode(context: XPathContext): StructuredQName = nameCode
}

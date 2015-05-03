// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.trans

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.Procedure
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.lib.StringCollator
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.pattern.Pattern
import org.orbeon.darius.xpath.`type`.AtomicType
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Corresponds to a single xsl:key declaration.<P>
 * @author Michael H. Kay
 */
class KeyDefinition(@BeanProperty var `match`: Pattern, 
    @BeanProperty var use: Expression, 
    @BeanProperty var collationName: String, 
    @BeanProperty var collation: StringCollator) extends Procedure {

  private var useType: AtomicType = _

  @BooleanBeanProperty
  var backwardsCompatible: Boolean = false

  @BooleanBeanProperty
  var convertUntypedToOther: Boolean = false

  setBody(use)

  /**
   * Set the primitive item type of the values returned by the use expression
   * @param itemType the primitive type of the indexed values
   */
  def setIndexedItemType(itemType: AtomicType): Unit = {
    useType = itemType
  }

  /**
   * Get the primitive item type of the values returned by the use expression
   * @return the primitive item type of the indexed values
   */
  def getIndexedItemType(): AtomicType = {
    if (useType == null) {
      AtomicType.ANY_ATOMIC
    } else {
      useType
    }
  }

  /**
   * Set the body of the key (the use expression). This is held redundantly as an Expression and
   * as a SequenceIterable (not sure why!)
   * @param body the use expression of the key
   */
  def setBody(body: Expression): Unit = {
    super.setBody(body)
    use = body
  }

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   *
   */
  def getObjectName(): StructuredQName = null

  def getConstructType(): StructuredQName = {
    new StructuredQName("xsl", NamespaceConstant.XSLT, "key")
  }

  override def getSystemId(): String = null

  override def getLineNumber(): Int = 0

  override def getProperty(name: String): AnyRef = null
}

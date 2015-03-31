// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.{AtomicType, TypeHierarchy}
import client.net.sf.saxon.ce.expr.{Expression, ExpressionVisitor, StaticProperty, XPathContext}
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.value.DateTimeValue

/**
 * This class implements the XPath 2.0 functions
 * current-date(), current-time(), and current-dateTime(), as
 * well as the function implicit-timezone(). The value that is required
 * is inferred from the type of result required.
 */
class CurrentDateTime extends SystemFunction {

  def newInstance(): CurrentDateTime = new CurrentDateTime()

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * (because the value of the expression depends on the runtime context)
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Determine the dependencies
   */
  override def getIntrinsicDependencies: Int = {
    StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
  }

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(context: XPathContext): Item = {
    val dt = DateTimeValue.getCurrentDateTime(context)
    val th = TypeHierarchy.getInstance
    val targetType = getItemType.asInstanceOf[AtomicType]
    if (targetType == AtomicType.DATE_TIME) {
      dt
    } else if (targetType == AtomicType.DATE) {
      dt.convert(AtomicType.DATE).asAtomic()
    } else if (targetType == AtomicType.TIME) {
      dt.convert(AtomicType.TIME).asAtomic()
    } else if (targetType == AtomicType.DAY_TIME_DURATION || targetType == AtomicType.DAY_TIME_DURATION) {
      dt.getComponent(Component.TIMEZONE)
    } else {
      throw new IllegalArgumentException("Wrong target type for current date/time")
    }
  }
}

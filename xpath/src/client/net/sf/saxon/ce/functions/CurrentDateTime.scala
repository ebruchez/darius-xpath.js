package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.DateTimeValue
//remove if not needed
import scala.collection.JavaConversions._

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
  def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = {
    StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
  }

  /**
   * Evaluate in a general context
   */
  def evaluateItem(context: XPathContext): Item = {
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

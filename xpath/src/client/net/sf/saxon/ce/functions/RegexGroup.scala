package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.regex.ARegexIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.NumericValue
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

class RegexGroup extends SystemFunction {

  def newInstance(): RegexGroup = new RegexGroup()

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    val gp0 = argument(0).evaluateItem(c).asInstanceOf[AtomicValue]
    val gp = gp0.asInstanceOf[NumericValue]
    val iter = c.getCurrentRegexIterator
    if (iter == null) {
      return StringValue.EMPTY_STRING
    }
    val s = iter.getRegexGroup(gp.intValue())
    if (s == null) {
      return StringValue.EMPTY_STRING
    }
    StringValue.makeStringValue(s)
  }

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_REGEX_GROUP
}

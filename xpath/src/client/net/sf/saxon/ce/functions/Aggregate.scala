package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.ExpressionTool
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This abstract class provides functionality common to the sum(), avg(), count(),
 * exists(), and empty() functions. These all take a sequence as input and produce a singleton
 * as output; and they are all independent of the ordering of the items in the input.
 */
abstract class Aggregate extends SystemFunction {

  /**
   * Static analysis: prevent sorting of the argument
   */
  def checkArguments(visitor: ExpressionVisitor) {
    super.checkArguments(visitor)
    argument(0) = ExpressionTool.unsorted(visitor.getConfiguration, argument(0), true)
  }
}

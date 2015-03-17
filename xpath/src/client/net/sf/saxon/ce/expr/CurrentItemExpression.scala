package client.net.sf.saxon.ce.expr

//remove if not needed
import scala.collection.JavaConversions._

/**
 *  The expression is generated when compiling the current() function in XSLT. It differs from
 *  the ContextItemExpression "." only in the error code that is returned when there is no context item.
 */
class CurrentItemExpression extends ContextItemExpression {

  /**
   * Get the error code for use when there is no context item
   * @return the string "XTDE1360"
   */
  protected def getErrorCodeForUndefinedContext(): String = "XTDE1360"
}

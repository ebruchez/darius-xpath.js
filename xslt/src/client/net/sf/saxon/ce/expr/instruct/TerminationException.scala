package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An exception thrown by xsl:message terminate="yes".
 */
class TerminationException(message: String) extends XPathException(message) {

  setErrorCode("XTMM9000")
}

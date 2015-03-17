package client.net.sf.saxon.ce.om

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A QNameException represents an error condition whereby a QName (for example a variable
 * name or template name) is malformed
 */
class QNameException(var message: String) extends Exception {

  def getMessage(): String = message
}

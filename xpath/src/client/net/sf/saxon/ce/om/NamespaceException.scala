package client.net.sf.saxon.ce.om

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A NamespaceException represents an error condition whereby a QName (for example a variable
 * name or template name) uses a namespace prefix that is not declared
 */
class NamespaceException(var prefix: String) extends Exception {

  def getMessage(): String = {
    "Namespace prefix " + prefix + " has not been declared"
  }

  def getPrefix(): String = prefix
}

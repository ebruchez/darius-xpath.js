package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import NoOpenStartTagException._
//remove if not needed
import scala.collection.JavaConversions._

object NoOpenStartTagException {

  /**
   * Static factory method to create the exception
   * @param nodeKind the kind of node being created (attribute or namespace)
   * @param name the name of the node being created
   * @param parentIsDocument true if the nodes are being added to a document node (rather than an element)
   * @return the constructed exception object
   */
  def makeNoOpenStartTagException(nodeKind: Int, name: String, parentIsDocument: Boolean): NoOpenStartTagException = {
    var message: String = null
    var errorCode: String = null
    if (parentIsDocument) {
      val kind = (if (nodeKind == Type.ATTRIBUTE) "an attribute" else "a namespace")
      message = "Cannot create " + kind + " node (" + name + ") whose parent is a document node"
      errorCode = "XTDE0420"
    } else {
      val kind = (if (nodeKind == Type.ATTRIBUTE) "An attribute" else "A namespace")
      message = kind + " node (" + name + 
        ") cannot be created after the children of the containing element"
      errorCode = "XTDE0410"
    }
    val err = new NoOpenStartTagException(message)
    err.setErrorCode(errorCode)
    err
  }
}

/**
 * Exception indicating that an attribute or namespace node has been written when
 * there is no open element to write it to
 */
class NoOpenStartTagException(message: String) extends XPathException(message)

package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.DocumentPool
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AnyURIValue
import client.net.sf.saxon.ce.value.QNameValue
import client.net.sf.saxon.ce.value.StringValue
import NamePart._
//remove if not needed
import scala.collection.JavaConversions._

object NamePart {

  val NAME = 0

  val LOCAL_NAME = 1

  val NAMESPACE_URI = 2

  val GENERATE_ID = 3

  val DOCUMENT_URI = 4

  val NODE_NAME = 6

  def getDocumentURI(node: NodeInfo, c: XPathContext): AnyURIValue = {
    if (node.getNodeKind == Type.DOCUMENT) {
      val pool = c.getController.getDocumentPool
      var docURI = pool.getDocumentURI(node)
      if (docURI == null) {
        docURI = node.getSystemId
      }
      if (docURI == null) {
        null
      } else if ("" == docURI) {
        null
      } else {
        new AnyURIValue(docURI)
      }
    } else {
      null
    }
  }

  /**
   * Test whether an expression is a call on the generate-id() function
   * @param exp the expression to be tested
   * @return true if exp is a call on generate-id(), else false
   */
  def isGenerateIdFunction(exp: Expression): Boolean = {
    ((exp.isInstanceOf[NamePart]) && 
      exp.asInstanceOf[NamePart].operation == GENERATE_ID)
  }
}

/**
 * This class supports the name(), local-name(), and namespace-uri() functions
 * from XPath 1.0, and also the XSLT generate-id() function
 */
class NamePart(operation: Int) extends SystemFunction {

  this.operation = operation

  def newInstance(): NamePart = new NamePart(operation)

  /**
   * Determine the special properties of this expression. The generate-id()
   * function is a special case: it is considered creative if its operand
   * is creative, so that generate-id(f()) is not taken out of a loop
   */
  def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    if (operation == GENERATE_ID) {
      p & ~StaticProperty.NON_CREATIVE
    } else {
      p
    }
  }

  override def computeDependencies(): Int = super.computeDependencies()

  /**
   * Evaluate the function in a string context
   */
  def evaluateItem(c: XPathContext): Item = {
    val node = argument(0).evaluateItem(c).asInstanceOf[NodeInfo]
    if (node == null) {
      if (operation == NODE_NAME || operation == DOCUMENT_URI) {
        return null
      } else if (operation == NAMESPACE_URI) {
        return AnyURIValue.EMPTY_URI
      } else {
        return StringValue.EMPTY_STRING
      }
    }
    var s: String = null
    operation match {
      case NAME => s = node.getDisplayName
      case LOCAL_NAME => s = node.getLocalPart
      case NAMESPACE_URI => 
        var uri = node.getURI
        s = (if (uri == null) "" else uri)
        return new AnyURIValue(s)

      case GENERATE_ID => 
        var buffer = new FastStringBuffer(FastStringBuffer.TINY)
        node.generateId(buffer)
        buffer.condense()
        return new StringValue(buffer)

      case DOCUMENT_URI => return getDocumentURI(node, c)
      case NODE_NAME => 
        var nodeName = node.getNodeName
        return (if (nodeName == null) null else new QNameValue(nodeName))

      case _ => throw new UnsupportedOperationException("Unknown name operation")
    }
    new StringValue(s)
  }
}

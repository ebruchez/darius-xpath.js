package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.Whitespace
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract class for fixed and computed attribute constructor expressions
 */
abstract class AttributeCreator extends SimpleNodeConstructor {

  /**
   * Process the value of the node, to create the new node.
   * @param value the string value of the new node
   * @param context the dynamic evaluation context
   * @throws XPathException
   */
  def processValue(value: CharSequence, context: XPathContext) {
    val nameCode = evaluateNameCode(context)
    val out = context.getReceiver
    if (nameCode == StructuredQName.XML_ID) {
      value = Whitespace.collapseWhitespace(value)
    }
    out.attribute(nameCode, value)
  }
}

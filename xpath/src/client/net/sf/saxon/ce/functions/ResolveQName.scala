package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.QNameValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class supports the resolve-QName function in XPath 2.0
 */
class ResolveQName extends SystemFunction {

  def newInstance(): ResolveQName = new ResolveQName()

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg0 == null) {
      return null
    }
    val lexicalQName = arg0.getStringValue
    val element = argument(1).evaluateItem(context).asInstanceOf[NodeInfo]
    val resolver = new InscopeNamespaceResolver(element)
    var qName: StructuredQName = null
    try {
      qName = StructuredQName.fromLexicalQName(lexicalQName, resolver.getURIForPrefix("", true), resolver)
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
    new QNameValue(qName)
  }
}

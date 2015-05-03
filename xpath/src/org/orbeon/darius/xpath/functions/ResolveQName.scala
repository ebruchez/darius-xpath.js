// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.{AtomicValue, QNameValue}

/**
 * This class supports the resolve-QName function in XPath 2.0
 */
class ResolveQName extends SystemFunction {

  def newInstance(): ResolveQName = new ResolveQName()

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg0 == null) {
      return null
    }
    val lexicalQName = arg0.getStringValue
    val element = argument(1).evaluateItem(context).asInstanceOf[NodeInfo]
    val resolver = new InscopeNamespaceResolver(element)
    var qName: StructuredQName = null
    try {
      qName = StructuredQName.fromLexicalQName(lexicalQName, resolver.getURIForPrefix("", useDefault = true), resolver)
    } catch {
      case e: XPathException ⇒
        e.maybeSetLocation(getSourceLocator)
        throw e
    }
    new QNameValue(qName)
  }
}

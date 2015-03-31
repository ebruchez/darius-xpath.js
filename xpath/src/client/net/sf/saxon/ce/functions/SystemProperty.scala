// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.Version
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.SystemProperty._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.{Item, StructuredQName}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.StringValue

object SystemProperty {

  /**
   * Here's the real code:
   *
   * @param uri the namespace URI of the system property name
   * @param local the local part of the system property name
   * @return the value of the corresponding system property
   */
  def getProperty(uri: String, local: String): String = {
    if (uri == NamespaceConstant.XSLT) {
      if (local == "version") {
        return Version.getXSLVersionString
      } else if (local == "vendor") {
        return Version.getProductTitle
      } else if (local == "vendor-url") {
        return Version.getWebSiteAddress
      } else if (local == "product-name") {
        return Version.getProductName
      } else if (local == "product-version") {
        return Version.getProductVariantAndVersion
      } else if (local == "supports-serialization") {
        return "no"
      } else if (local == "supports-backwards-compatibility") {
        return "yes"
      } else if (local == "supports-namespace-axis") {
        return "yes"
      } else if (local == "is-schema-aware") {
        return "no"
      }
      ""
    } else {
      ""
    }
  }
}

/**
 * Implementation of the XSLT system-property() function
 */
class SystemProperty extends SystemFunction {

  def newInstance(): SystemProperty = new SystemProperty()

  private var staticContext: StaticContext = _

  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    if (staticContext == null) {
      staticContext = visitor.getStaticContext
    }
  }

  /**
   * Evaluate the function at run-time
   */
  override def evaluateItem(context: XPathContext): Item = {
    var qName: StructuredQName = null
    val name = argument(0).evaluateItem(context).getStringValue
    try {
      qName = StructuredQName.fromLexicalQName(name, "", staticContext.getNamespaceResolver)
    } catch {
      case err: XPathException => {
        dynamicError("Invalid system property name. " + err.getMessage, "XTDE1390")
        return null
      }
    }
    new StringValue(getProperty(qName.getNamespaceURI, qName.getLocalName))
  }
}

package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.Version
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.StringValue
import SystemProperty._
//remove if not needed
import scala.collection.JavaConversions._

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

  def checkArguments(visitor: ExpressionVisitor) {
    if (staticContext == null) {
      staticContext = visitor.getStaticContext
    }
  }

  /**
   * Evaluate the function at run-time
   */
  def evaluateItem(context: XPathContext): Item = {
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

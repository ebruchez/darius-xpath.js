package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.value.AnyURIValue
import client.net.sf.saxon.ce.value.AtomicValue
import ResolveURI._
//remove if not needed
import scala.collection.JavaConversions._

object ResolveURI {

  /**
   * If a system ID can't be parsed as a URL, try to expand it as a relative
   * URI using the current directory as the base URI.
   */
  def tryToExpand(systemId: String): String = systemId

  /**
   * Construct an absolute URI from a relative URI and a base URI. The method uses the resolve
   * method of the java.net.URI class, except where the base URI uses the (non-standard) "jar:" scheme,
   * in which case the method used is <code>new URL(baseURL, relativeURL)</code>.
   *
   * <p>Spaces in either URI are converted to %20</p>
   *
   * <p>If no base URI is available, and the relative URI is not an absolute URI, then the current
   * directory is used as a base URI.</p>
   *
   * @param relativeURI the relative URI. Null is permitted provided that the base URI is an absolute URI
   * @param base        the base URI. Null is permitted provided that relativeURI is an absolute URI
   * @return the absolutized URI
   * @throws java.net.URISyntaxException if either of the strings is not a valid URI or
   * if the resolution fails
   */
  def makeAbsolute(relativeURI: String, base: String): URI = {
    var absoluteURI: URI = null
    if (relativeURI == null) {
      absoluteURI = new URI(ResolveURI.escapeSpaces(base), true)
      if (!absoluteURI.isAbsolute) {
        throw new URI.URISyntaxException(base + 
          ": Relative URI not supplied, so base URI must be absolute")
      } else {
        return absoluteURI
      }
    }
    relativeURI = ResolveURI.escapeSpaces(relativeURI)
    base = ResolveURI.escapeSpaces(base)
    if (base == null || base.length == 0) {
      absoluteURI = new URI(relativeURI, true)
      if (!absoluteURI.isAbsolute) {
        val expandedBase = ResolveURI.tryToExpand(base)
        if (expandedBase != base) {
          return makeAbsolute(relativeURI, expandedBase)
        }
      }
    } else {
      var baseURI: URI = null
      baseURI = new URI(base)
      if (baseURI.getFragment != null) {
        val hash = base.indexOf('#')
        if (hash >= 0) {
          base = base.substring(0, hash)
        }
        baseURI = new URI(base)
      }
      new URI(relativeURI, true)
      absoluteURI = (if (relativeURI.length == 0) baseURI else baseURI.resolve(relativeURI))
    }
    absoluteURI
  }

  /**
   * Replace spaces by %20
   */
  def escapeSpaces(s: String): String = {
    if (s == null) return s
    val i = s.indexOf(' ')
    if (i < 0) {
      return s
    }
    (if (i == 0) "" else s.substring(0, i)) + "%20" + 
      (if (i == s.length - 1) "" else escapeSpaces(s.substring(i + 1)))
  }
}

/**
 * This class supports the resolve-uri() functions in XPath 2.0
 */
class ResolveURI extends SystemFunction {

  def newInstance(): ResolveURI = new ResolveURI()

  var expressionBaseURI: String = null

  def checkArguments(visitor: ExpressionVisitor) {
    if (expressionBaseURI == null) {
      super.checkArguments(visitor)
      expressionBaseURI = visitor.getStaticContext.getBaseURI
      if (expressionBaseURI == null && argument.length == 1) {
        val de = new XPathException("Base URI in static context of resolve-uri() is unknown")
        de.setErrorCode("FONS0005")
        throw de
      }
    }
  }

  /**
   * Get the static base URI of the expression
   */
  def getStaticBaseURI(): String = expressionBaseURI

  /**
   * Evaluate the function at run-time
   */
  def evaluateItem(context: XPathContext): Item = {
    val arg0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (arg0 == null) {
      return null
    }
    val relative = arg0.getStringValue
    val msgBase = "in resolve-uri(), Base URI "
    var base: String = null
    if (argument.length == 2) {
      base = argument(1).evaluateAsString(context).toString
    } else {
      base = expressionBaseURI
      if (expressionBaseURI == null) {
        dynamicError(msgBase + "in static context of resolve-uri() is unknown", "FONS0005")
        return null
      }
    }
    try {
      val absoluteURI = new URI(base, true)
      if (!absoluteURI.isAbsolute) {
        val relativeURI = new URI(relative, true)
        if (relativeURI.isAbsolute) {
          return new AnyURIValue(relative)
        }
        dynamicError(msgBase + Err.wrap(base) + " is not an absolute URI", "FORG0002")
        return null
      }
      val resolved = makeAbsolute(relative, base)
      new AnyURIValue(resolved.toString)
    } catch {
      case err: URI.URISyntaxException => {
        dynamicError(msgBase + Err.wrap(base) + " is invalid: " + err.getMessage, "FORG0002")
        null
      }
    }
  }
}

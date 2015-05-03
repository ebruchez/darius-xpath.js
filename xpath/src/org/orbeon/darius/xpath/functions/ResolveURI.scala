// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import java.net.{URI, URISyntaxException}

import org.orbeon.darius.xpath.expr.{ExpressionVisitor, XPathContext}
import org.orbeon.darius.xpath.functions.ResolveURI._
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.trans.{Err, XPathException}
import org.orbeon.darius.xpath.value.{AnyURIValue, AtomicValue}

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
   * @param _relativeURI the relative URI. Null is permitted provided that the base URI is an absolute URI
   * @param _base        the base URI. Null is permitted provided that relativeURI is an absolute URI
   * @return the absolutized URI
   * @throws java.net.URISyntaxException if either of the strings is not a valid URI or
   * if the resolution fails
   */
  def makeAbsolute(_relativeURI: String, _base: String): URI = {
    var relativeURI = _relativeURI
    var base = _base
    var absoluteURI: URI = null
    if (relativeURI == null) {
      absoluteURI = new URI(ResolveURI.escapeSpaces(base))//ORBEON true
      if (!absoluteURI.isAbsolute) {
        throw new URISyntaxException(base, "Relative URI not supplied, so base URI must be absolute")
      } else {
        return absoluteURI
      }
    }
    relativeURI = ResolveURI.escapeSpaces(relativeURI)
    base = ResolveURI.escapeSpaces(base)
    if (base == null || base.length == 0) {
      absoluteURI = new URI(relativeURI)//ORBEON true
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
      new URI(relativeURI)//ORBEON true
      absoluteURI = if (relativeURI.length == 0) baseURI else baseURI.resolve(relativeURI)
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

  override def checkArguments(visitor: ExpressionVisitor): Unit = {
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
  def getStaticBaseURI: String = expressionBaseURI

  /**
   * Evaluate the function at run-time
   */
  override def evaluateItem(context: XPathContext): Item = {
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
      val absoluteURI = new URI(base)//ORBEON true
      if (!absoluteURI.isAbsolute) {
        val relativeURI = new URI(relative)//ORBEON true
        if (relativeURI.isAbsolute) {
          return new AnyURIValue(relative)
        }
        dynamicError(msgBase + Err.wrap(base) + " is not an absolute URI", "FORG0002")
        return null
      }
      val resolved = makeAbsolute(relative, base)
      new AnyURIValue(resolved.toString)
    } catch {
      case err: URISyntaxException ⇒
        dynamicError(msgBase + Err.wrap(base) + " is invalid: " + err.getMessage, "FORG0002")
        null
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.om.NamespaceException
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.DecimalFormatManager
import client.net.sf.saxon.ce.trans.DecimalSymbols
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.StringValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:decimal-format elements in stylesheet. <br>
 */
class XSLDecimalFormat extends StyleElement {

  var prepared: Boolean = false

  var name: String = _

  var decimalSeparator: String = _

  var groupingSeparator: String = _

  var infinity: String = _

  var minusSign: String = _

  var NaN: String = _

  var percent: String = _

  var perMille: String = _

  var zeroDigit: String = _

  var digit: String = _

  var patternSeparator: String = _

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes(): Unit = {
    if (prepared) {
      return
    }
    prepared = true
    setObjectName(checkAttribute("name", "q1").asInstanceOf[StructuredQName])
    decimalSeparator = checkAttribute("decimal-separator", "s").asInstanceOf[String]
    groupingSeparator = checkAttribute("grouping-separator", "s").asInstanceOf[String]
    infinity = checkAttribute("infinity", "s").asInstanceOf[String]
    minusSign = checkAttribute("minus-sign", "s").asInstanceOf[String]
    NaN = checkAttribute("NaN", "s").asInstanceOf[String]
    percent = checkAttribute("percent", "s").asInstanceOf[String]
    perMille = checkAttribute("per-mille", "s").asInstanceOf[String]
    digit = checkAttribute("digit", "s").asInstanceOf[String]
    patternSeparator = checkAttribute("pattern-separator", "s").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    checkTopLevel(null)
    checkEmpty()
  }

  def makeDecimalFormatSymbols(): DecimalSymbols = {
    val d = new DecimalSymbols()
    if (decimalSeparator != null) {
      d.decimalSeparator = toChar(decimalSeparator)
    }
    if (groupingSeparator != null) {
      d.groupingSeparator = toChar(groupingSeparator)
    }
    if (infinity != null) {
      d.infinity = infinity
    }
    if (minusSign != null) {
      d.minusSign = toChar(minusSign)
    }
    if (NaN != null) {
      d.NaN = NaN
    }
    if (percent != null) {
      d.percent = toChar(percent)
    }
    if (perMille != null) {
      d.permill = toChar(perMille)
    }
    if (zeroDigit != null) {
      d.zeroDigit = toChar(zeroDigit)
      if (!d.isValidZeroDigit) {
        compileError("The value of the zero-digit attribute must be a Unicode digit with value zero", 
          "XTSE1295")
      }
    }
    if (digit != null) {
      d.digit = toChar(digit)
    }
    if (patternSeparator != null) {
      d.patternSeparator = toChar(patternSeparator)
    }
    try {
      d.checkDistinctRoles()
    } catch {
      case err: XPathException ⇒ compileError(err.getMessage, "XTSE1300")
    }
    d
  }

  /**
   * Method supplied by declaration elements to add themselves to a stylesheet-level index
   * @param decl the Declaration being indexed. (This corresponds to the StyleElement object
   * except in cases where one module is imported several times with different precedence.)
   * @param top  the outermost XSLStylesheet element
   */
  def index(decl: Declaration, top: PrincipalStylesheetModule): Unit = {
    prepareAttributes()
    val d = makeDecimalFormatSymbols()
    val dfm = getPreparedStylesheet.getDecimalFormatManager
    if (name == null) {
      try {
        dfm.setDefaultDecimalFormat(d, decl.getPrecedence)
      } catch {
        case err: XPathException ⇒ compileError(err.getMessage, err.getErrorCodeQName)
      }
    } else {
      try {
        val formatName = makeQName(name)
        try {
          dfm.setNamedDecimalFormat(formatName, d, decl.getPrecedence)
        } catch {
          case err: XPathException ⇒ compileError(err.getMessage, err.getErrorCodeQName)
        }
      } catch {
        case err: XPathException ⇒ compileError("Invalid decimal format name. " + err.getMessage, "XTSE0020")
        case err: NamespaceException ⇒ compileError("Invalid decimal format name. " + err.getMessage,
          "XTSE0280")
      }
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = null

  /**
   * Get the Unicode codepoint corresponding to a String, which must represent a single Unicode character
   * @param s the input string, representing a single Unicode character, perhaps as a surrogate pair
   * @return
   * @throws XPathException
   */
  private def toChar(s: String): Int = {
    val e = StringValue.expand(s)
    if (e.length != 1) compileError("Attribute \"" + s + "\" should be a single character", "XTSE0020")
    e(0)
  }
}

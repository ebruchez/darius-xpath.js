// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.tree.util.SourceLocator

import scala.beans.BeanProperty

/**
 * Subclass of XPathException used to report circularities
 */
class Circularity(message: String) extends XPathException(message)

/**
 * XPathException is used to indicate an error in an XPath expression.
 * It will generally be either a StaticError or a DynamicError;
 * ValidationExceptions (arising from schema validation) form a third category
 */
class XPathException(_message: String, _throwable: Throwable) extends Exception(_message, _throwable) {

  var isTypeError = false
  var isStaticError = false

  private var locationText: String = null
  private var errorCode: StructuredQName = _

  private var _hasBeenReported = false
  def hasBeenReported() = _hasBeenReported

  @BeanProperty
  var locator: SourceLocator = _

  def this(message: String) =
    this(message, null: Throwable)

  def this() =
    this(null: String)

  /**
   * Create an XPathException that wraps another exception
   * @param err the wrapped error or exception
   */
  def this(err: Throwable) =
    this(null, err)

  /**
   * Create an XPathException that supplies an error message and supplies location information
   * @param message the error message
   * @param loc indicates where in the user-written query or stylesheet (or sometimes in a source
   * document) the error occurred
   */
  def this(message: String, loc: SourceLocator) {
    this(message)
    this.locator = loc
  }

  override def toString(): String = getMessage

  /**
   * Create an XPathException that supplies an error message and an error code
   * @param message the error message
   * @param errorCode the error code - an eight-character code, which is taken to be in the standard
   * system error code namespace
   */
  def this(message: String, errorCode: String) {
    this(message)
    setErrorCode(errorCode)
    if (errorCode == "XPTY0004") {
      setIsTypeError(true)
    }
  }

  /**
   * Create an XPathException that supplies an error message and an error code and a locator
   * @param message the error message
   * @param errorCode the error code - an eight-character code, which is taken to be in the standard
   * system error code namespace
   * @param loc indicates where in the user-written query or stylesheet (or sometimes in a source
   * document) the error occurred
   */
  def this(message: String, errorCode: String, loc: SourceLocator) {
    this(message)
    setErrorCode(errorCode)
    this.locator = loc
  }

  /**
   * Force an exception to a static error
   * @return this exception, marked as a static error
   */
  def makeStatic(): XPathException = {
    setIsStaticError(true)
    this
  }

  /**
   * Set additional location text. This gives extra information about the position of the error
   * in textual form. Where XPath is embedded within a host language such as XSLT, the
   * formal location information identifies the location of the error in the XSLT module,
   * while this string locates the error within a specific XPath expression. The information
   * is typically used only for static errors.
   * @param text additional information about the location of the error, designed to be output
   * as a prefix to the error message if desired. (It is not concatenated with the message, because
   * it may be superfluous in an IDE environment.)
   */
  def setAdditionalLocationText(text: String): Unit = {
    locationText = text
  }

  /**
   * Get the additional location text, if any. This gives extra information about the position of the error
   * in textual form. Where XPath is embedded within a host language such as XSLT, the
   * formal location information identifies the location of the error in the XSLT module,
   * while this string locates the error within a specific XPath expression. The information
   * is typically used only for static errors.
   * @return additional information about the location of the error, designed to be output
   * as a prefix to the error message if desired. (It is not concatenated with the message, because
   * it may be superfluous in an IDE environment.)
   */
  def getAdditionalLocationText(): String = locationText

  /**
   * Mark this exception to indicate that it represents (or does not represent) a static error
   * @param is true if this exception is a static error
   */
  def setIsStaticError(is: Boolean): Unit = {
    isStaticError = is
  }

  /**
   * Mark this exception to indicate that it represents (or does not represent) a type error
   * @param is true if this exception is a type error
   */
  def setIsTypeError(is: Boolean): Unit = {
    isTypeError = is
  }

  /**
   * Set the error code. The error code is a QName; this method sets the local part of the name,
   * setting the namespace of the error code to the standard system namespace [[client.net.sf.saxon.ce.lib.NamespaceConstant.ERR]]
   * @param code The local part of the name of the error code
   */
  def setErrorCode(code: String): Unit = {
    if (code != null) {
      errorCode = new StructuredQName("err", NamespaceConstant.ERR, code)
    }
  }

  /**
   * Set the error code, provided it has not already been set.
   * The error code is a QName; this method sets the local part of the name,
   * setting the namespace of the error code to the standard system namespace [[NamespaceConstant.ERR]]
   * @param code The local part of the name of the error code
   */
  def maybeSetErrorCode(code: String): Unit = {
    if (errorCode == null && code != null) {
      errorCode = new StructuredQName("err", NamespaceConstant.ERR, code)
    }
  }

  /**
   * Set the error code. The error code is a QName; this method sets both parts of the name.
   * @param code The error code as a QName
   */
  def setErrorCodeQName(code: StructuredQName): Unit = {
    errorCode = code
  }

  /**
   * Get the error code as a QName
   * @return the error code as a QName
   */
  def getErrorCodeQName(): StructuredQName = errorCode

  /**
   * Get the local part of the name of the error code
   * @return the local part of the name of the error code
   */
  def getErrorCodeLocalPart(): String = {
    (if (errorCode == null) null else errorCode.getLocalName)
  }

  /**
   * Get the namespace URI part of the name of the error code
   * @return the namespace URI part of the name of the error code
   */
  def getErrorCodeNamespace(): String = {
    (if (errorCode == null) null else errorCode.getNamespaceURI)
  }

  /**
   * Mark this error to indicate that it has already been reported to the error listener, and should not be
   * reported again
   * @param reported true if the error has been reported to the error listener
   */
  def setHasBeenReported(reported: Boolean): Unit = {
    _hasBeenReported = reported
  }

  /**
   * Set the location of a message, only if it is not already set
   * @param locator the current location (or null)
   */
  def maybeSetLocation(locator: SourceLocator): Unit = {
    if (this.locator == null) {
      this.locator = locator
    }
  }

  /**
   * Tests whether this is a dynamic error that may be reported statically if it is detected statically
   * @return true if the error can be reported statically
   */
  def isReportableStatically(): Boolean = {
    if (isStaticError || isTypeError) {
      return true
    }
    if (errorCode != null && errorCode.getNamespaceURI == NamespaceConstant.ERR) {
      val local = errorCode.getLocalName
      return local == "XTDE1260" || local == "XTDE1280" || local == "XTDE1390" || 
        local == "XTDE1400" || 
        local == "XDTE1428" || 
        local == "XTDE1440" || 
        local == "XTDE1460"
    }
    false
  }
}

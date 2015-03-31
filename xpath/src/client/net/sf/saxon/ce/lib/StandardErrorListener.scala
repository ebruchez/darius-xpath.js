// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.lib

import java.io.PrintStream
import java.util.logging.{Level, Logger}

import client.net.sf.saxon.ce.LogConfiguration
import client.net.sf.saxon.ce.lib.StandardErrorListener._
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.SourceLocator

object StandardErrorListener {

  private val logger: Logger = Logger.getLogger("StandardErrorListener")

  private def getLocationMessageText(loc: SourceLocator): String = "at " + loc.getLocation

  /**
   * Abbreviate a URI (if requested)
   * @param uri the URI to be abbreviated
   * @return the abbreviated URI, unless full path names were requested, in which case
   * the URI as supplied
   */
  def abbreviatePath(uri: String): String = {
    if (uri == null) {
      return null
    }
    val slash = uri.lastIndexOf('/')
    if (slash >= 0 && slash < uri.length - 1) {
      uri.substring(slash + 1)
    } else {
      uri
    }
  }

  private def getCodeMessage(qCode: StructuredQName): String = {
    var codeText = ""
    if (qCode != null) {
      var code = qCode.getLocalName
      if (code.startsWith("XTTE")) {
        code = code.substring(4)
        val q = Integer.parseInt(code)
        val suffix = " must match its declared type"
        q match {
          case 570 => codeText = " The value of a variable" + suffix
          case 600 => codeText = " Default value of a template paremeter" + suffix
          case 590 => codeText = " Supplied value of a template parameter" + suffix
          case _ =>
        }
      } else if (code.startsWith("XPTY")) {
        code = code.substring(4)
        val q = Integer.parseInt(code)
        q match {
          case 4 => codeText = " The expression value is not consistent with the context in which it appears"
          case 18 => codeText = " Last step in path expression contains both nodes and atomic values"
          case 19 => codeText = " A path expression step contains an atomic value"
          case 20 => codeText = " In an axis step, the context item is not a node"
          case _ =>
        }
      }
    }
    codeText
  }

  /**
   * Get a string containing the message for this exception and all contained exceptions
   *
   * @param err the exception containing the required information
   * @return a message that concatenates the message of this exception with its contained exceptions,
   *         also including information about the error code and location.
   */
  def getExpandedMessage(err: XPathException): String = {
    var qCode: StructuredQName = null
    var additionalLocationText: String = null
    qCode = err.getErrorCodeQName
    additionalLocationText = err.getAdditionalLocationText
    if (qCode == null && err.getCause.isInstanceOf[XPathException]) {
      qCode = err.getCause.asInstanceOf[XPathException].getErrorCodeQName
    }
    var message = ""
    val codeText = ""
    if (qCode != null) {
      message = if (qCode.getNamespaceURI == NamespaceConstant.ERR) qCode.getLocalName else qCode.getDisplayName
    }
    if (additionalLocationText != null) {
      message += " " + additionalLocationText
    }
    var e: Throwable = err
    val msgLen = message.length
    while (true) {
      if (e == null) {
        //break
      }
      var next = e.getMessage
      if (next == null) {
        next = ""
      }
      if (next.startsWith("client.net.sf.saxon.ce.trans.XPathException: ")) {
        next = next.substring(next.indexOf(": ") + 2)
      }
      if (!message.endsWith(next)) {
        if ("" != message && !message.trim().endsWith(":")) {
          message += ": "
        }
        message += next
      }
      if (e.isInstanceOf[XPathException]) {
        e = e.getCause
      } else {
        //break
      }
    }
    if (LogConfiguration.loggingIsEnabled()) {
      if (msgLen == message.length) {
        val msg = getCodeMessage(qCode)
        if (msg.length != 0) {
          message += ": " + msg
        }
      }
    }
    message
  }

  /**
   * Wordwrap an error message into lines of 72 characters or less (if possible)
   *
   * @param message the message to be word-wrapped
   * @return the message after applying word-wrapping
   */
  private def wordWrap(message: String): String = {
    var nl = message.indexOf('\n')
    if (nl < 0) {
      nl = message.length
    }
    if (nl > 100) {
      var i = 90
      while (message.charAt(i) != ' ' && i > 0) {
        i -= 1
      }
      if (i > 10) {
        message.substring(0, i) + "\n  " + wordWrap(message.substring(i + 1))
      } else {
        message
      }
    } else if (nl < message.length) {
      message.substring(0, nl) + '\n' + wordWrap(message.substring(nl + 1))
    } else {
      message
    }
  }
}

/**
 * <B>StandardErrorListener</B> is the standard error handler for XSLT and XQuery processing
 * errors, used if no other ErrorListener is nominated.
 *
 * @author Michael H. Kay
 */
class StandardErrorListener extends ErrorListener {

  private var warningCount: Int = 0

  protected var errorOutput: PrintStream = System.err

  /**
   * Make a clean copy of this ErrorListener. This is necessary because the
   * standard error listener is stateful (it remembers how many errors there have been)
   *
   * @return a copy of this error listener
   */
  def makeAnother(): StandardErrorListener = {
    val sel = new StandardErrorListener()
    sel.errorOutput = errorOutput
    sel
  }

  /**
   * Set output destination for error messages (default is System.err)
   *
   * @param writer The PrintStream to use for error messages
   */
  def setErrorOutput(writer: PrintStream): Unit = {
    errorOutput = writer
  }

  /**
   * Get the error output stream
   *
   * @return the error output stream
   */
  def getErrorOutput(): PrintStream = errorOutput

  /**
   * Receive notification of a warning.
   * <p/>
   * <p>Transformers can use this method to report conditions that
   * are not errors or fatal errors.  The default behaviour is to
   * take no action.</p>
   * <p/>
   * <p>After invoking this method, the Transformer must continue with
   * the transformation. It should still be possible for the
   * application to process the document through to the end.</p>
   *
   * @param exception The warning information encapsulated in a
   *                  transformer exception.
   * @see javax.xml.transform.TransformerException
   */
  def warning(exception: XPathException): Unit = {
    if (errorOutput == null) {
      errorOutput = System.err
    }
    var message = ""
    if (exception.getLocator != null) {
      message = getLocationMessage(exception) + "\n  "
    }
    message += wordWrap(getExpandedMessage(exception))
    logger.log(Level.WARNING, message)
    errorOutput.println("Warning: " + message)
    warningCount += 1
    if (warningCount > 25) {
      errorOutput.println("No more warnings will be displayed")
      warningCount = 0
    }
  }

  /**
   * Receive notification of a non-recoverable error.
   * <p/>
   * <p>The application must assume that the transformation cannot
   * continue after the Transformer has invoked this method,
   * and should continue (if at all) only to collect
   * addition error messages. In fact, Transformers are free
   * to stop reporting events once this method has been invoked.</p>
   *
   * @param exception The error information encapsulated in a
   *                  transformer exception.
   * @throws XPathException if the application
   *                              chooses to discontinue the transformation.
   */
  def error(exception: XPathException): Unit = {
    if (exception.hasBeenReported()) {
      return
    }
    if (errorOutput == null) {
      errorOutput = System.err
    }
    val message = "Error " + getLocationMessage(exception) + "\n  " + wordWrap(getExpandedMessage(exception))
    logger.log(Level.SEVERE, message)
    errorOutput.println(message)
    if (exception.isInstanceOf[XPathException]) {
      exception.setHasBeenReported(true)
    }
  }

  /**
   * Get a string identifying the location of an error.
   *
   * @param _err the exception containing the location information
   * @return a message string describing the location
   */
  def getLocationMessage(_err: XPathException): String = {
    var err = _err
    var loc = err.getLocator
    while (loc == null) {
      if (err.getCause.isInstanceOf[XPathException]) {
        err = err.getCause.asInstanceOf[XPathException]
        loc = err.getLocator
      } else {
        return ""
      }
    }
    getLocationMessageText(loc)
  }
}

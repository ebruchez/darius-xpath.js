package client.net.sf.saxon.ce

import client.net.sf.saxon.ce.dom.XMLDOM
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.dom.client.Document
import com.google.gwt.logging.client.LogConfiguration
import com.google.gwt.user.client.Window
import java.util.logging.Logger
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object SaxonceApi {

  private var processorWasJsInitiated: Boolean = false

  def setProcessorWasJsInitiated() {
    processorWasJsInitiated = true
  }

  /**
   * @return boolean to indicated to logging and other code that the
   *         XSLTProcessor was initiated from the JavaScript API
   */
  def doThrowJsExceptions(): Boolean = {
    processorWasJsInitiated && (handler == null)
  }

  def requestXML(URI: String): JavaScriptObject = {
    val pageHref = Window.Location.getHref
    val absSourceURI = (new URI(pageHref).resolve(URI)).toString
    createAsyncDoc(absSourceURI)
  }

  /**
   * Returns a DocumentInfo object that wraps a XML DOM document.
   *
   * If the JavaScript object passed as a parameter is not a DOM document, but
   * simply a place-holder, then the Document is first fetched synchronously
   * before wrapping.
   *
   * @param obj
   *            the DOM document or a place-holder
   * @param config
   *            The Saxon-CE configuration
   * @return a DocumentInfo object
   */
  def getDocSynchronously(obj: JavaScriptObject, config: Configuration): DocumentInfo = {
    val absSourceURI = getAsyncUri(obj)
    var doc: Document = null
    if (absSourceURI != null) {
      val xml = XMLDOM.makeHTTPRequest(absSourceURI)
      doc = XMLDOM.parseXML(xml).asInstanceOf[Document]
    } else {
      doc = obj.asInstanceOf[Document]
    }
    if (doc.getDocumentElement == null) {
      throw new XPathException("no document element")
    }
    config.wrapXMLDocument(doc, absSourceURI)
  }

  /**
   * Registers static methods of this SaxonApi class and the LogController
   * class as static methods for use in the JavaScript API, within the Saxonce
   * namespace. This method must be called when the Saxonce GWT module is
   * first loaded.
   */
  /* native */ def register(): Unit

  /**
   * Factory method for JavaScript API to create new XSLT20Processor Converts
   * this to new Saxonce.XSLT20Processor(doc)
   */
  /* native */ def newXSLT20Processor(doc: JavaScriptObject): JavaScriptObject

  private var handler: JavaScriptObject = null

  /**
   * API call to set the function used by <code>initCallback()</code> to make
   * a JavaScript callback when a logging event occurs
   *
   * @param handlerFunction
   *            - An instance of a JavaScript function
   */
  def setErrorHandler(handlerFunction: JavaScriptObject) {
    handler = handlerFunction
  }

  /**
   * Public API function - but also used internally Return the function object
   * thats the error handler. This is the external error handler (set by a
   * hosting editor for example) of if there is none then the error handler
   * set using <code>setErrorHandler()
   */
  def getErrorHandler(): JavaScriptObject = handler

  def setAnyExternalErrorHandler() {
    logHandlerExternal = callExternalErrorHandler(Version.getProductTitle, "INIT")
  }

  @BooleanBeanProperty
  var logHandlerExternal: Boolean = false

  private /* native */ def callExternalErrorHandler(message: String, level: String): Boolean

  /**
   * Calls back into the JavaScript calling code to allow logging of errors
   * and events from within JavaScript
   *
   * @param message
   *            - The error message
   * @param errorType
   *            - The error type - can be any type used by GWT-Logging
   */
  def makeCallback(message: String, errorType: String, milliseconds: String) {
    val currentHandler = getErrorHandler
    setErrorMessage(message)
    if (currentHandler == null && !isLogHandlerExternal) return
    if (!callbackErrorReported) {
      var success = false
      if (currentHandler != null) {
        val evt = createEventObject(message, errorType, milliseconds)
        success = initCallback(currentHandler, evt)
        logAnyCallbackError(success, "JS")
      }
      if (isLogHandlerExternal) {
        success = callExternalErrorHandler(message, errorType)
        logAnyCallbackError(success, "Ext")
      }
    }
  }

  def logAnyCallbackError(success: Boolean, name: String) {
    if (LogConfiguration.loggingIsEnabled() && !success) {
      callbackErrorReported = true
      Logger.getLogger("HandlerCallback").severe("Exception on " + name + " errorHandler callback")
    }
  }

  var callbackErrorReported: Boolean = false

  /**
   * Creates an event object for use in the JavaScript API callback
   *
   * @param message
   *            The event message
   * @param errorType
   *            The event type e.g. FINE
   * @return the event object
   */
  private /* native */ def createEventObject(message: String, errorType: String, milliseconds: String): JavaScriptObject

  private /* native */ def initCallback(handlerFn: JavaScriptObject, eventObj: JavaScriptObject): Boolean

  private /* native */ def setErrorMessage(msg: String): Unit

  /* native */ def createAsyncDoc(URI: String): JavaScriptObject

  /* native */ def getAsyncUri(obj: JavaScriptObject): String

  /* native */ def runCommand(cmd: JavaScriptObject): JavaScriptObject
}

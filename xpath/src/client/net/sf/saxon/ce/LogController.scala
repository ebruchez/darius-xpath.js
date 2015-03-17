package client.net.sf.saxon.ce

import client.net.sf.saxon.ce.lib.GenericLogHandler
import client.net.sf.saxon.ce.lib.JsLogHandler
import client.net.sf.saxon.ce.lib.ListenerLogHandler
import client.net.sf.saxon.ce.lib.TraceListener
import client.net.sf.saxon.ce.trace.XSLTTraceListener
import com.google.gwt.logging.client.HasWidgetsLogHandler
import com.google.gwt.logging.client.LoggingPopup
import com.google.gwt.user.client.Window
import com.google.gwt.user.client.ui.HasWidgets
import java.util.logging.Level
import java.util.logging.LogRecord
import java.util.logging.Logger
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object LogController {

  /**
   * Class for managing the GWT Logging
   * Proposed use of levels:
   * - OFF
   - SEVERE GWT Exceptions and internal errors all fatal xslt errors
   - WARNING GWT Warnings and bad but not fatal internal conditions
   - INFO xsl:message and fn:trace output
   - CONFIG Configuration data
   - FINE High-level XSLT calls to the public API = Main transformation, inward calls such as events
   - FINER XSLT/XPath/IXSL function and template calls
   - FINEST Output from TraceExpression - XSLT instructions within templates and functions
   
   SystemLogHandler - not included
   HasWidgetsLogHandler - the popup view
   DevelopmentModeLogHandler
   ConsoleLogHandler - IE or FireBug Lite consoles
   SimpleRemoteLogHandler
   FirebugLogHandler
   -----------------
   GenericLogHandler - in-house mod improves on ConsoleLogHandler + FirebugHandler
   JSLogHandler - Raises JavaScript event for each log item
   
   Set properties in Saxonce.gwt.xml to control logging: e.g.
   
   <set-property name="gwt.logging.logLevel" value="SEVERE"/>          # To change the default logLevel
   <set-property name="gwt.logging.enabled" value="FALSE"/>            # To disable logging
   <set-property name="gwt.logging.consoleHandler" value="DISABLED"/>  # To disable a default Handler
   <set-property name="gwt.logging.popupHandler" value="DISABLED"/>    # To disable the popupHandler
   */
  private var isTraceEnabled: Boolean = _

  private var mainLogger: Logger = _

  private var initLogLevel: Level = _

  def initLogger() {
    mainLogger = Logger.getLogger("")
    val logLevel = Window.Location.getParameter("logLevel")
    initLogLevel = if ((logLevel == null)) null else Level.parse(logLevel)
  }

  def LoggingIsDisabledByURI(): Boolean = mainLogger.getLevel == Level.OFF

  @BeanProperty
  var traceListener: TraceListener = null

  def InitializeTraceListener() {
    checkTraceIsEnabled()
    if (isTraceEnabled) {
      traceListener = new XSLTTraceListener()
    }
  }

  def openTraceListener() {
    if (isTraceEnabled) {
      traceListener.open()
    }
  }

  def closeTraceListener(success: Boolean) {
    if (traceListener != null) {
      if (success) {
        if (isTraceEnabled) {
          traceListener.close()
        }
      } else {
        traceListener.asInstanceOf[XSLTTraceListener].terminate()
      }
    }
  }

  def traceIsEnabled(): Boolean = isTraceEnabled

  private def checkTraceIsEnabled(): Boolean = {
    isTraceEnabled = mainLogger.getLevel == Level.FINEST
    isTraceEnabled
  }

  def addJavaScriptLogHandler() {
    if (!LoggingIsDisabledByURI()) {
      Logger.getLogger("").addHandler(new ListenerLogHandler())
    }
  }

  @BeanProperty
  var jsLogHandler: JsLogHandler = null

  def setLogLevel(newLevel: String) {
    if (initLogLevel == null) {
      try {
        mainLogger.setLevel(Level.parse(newLevel))
      } catch {
        case e: Exception => Logger.getLogger("LogController").severe("invalid level for setLogLevel: " + newLevel)
      }
    }
  }

  def getLogLevel(): String = mainLogger.getLevel.getName

  def addRequiredLogHanders(record: LogRecord) {
    jsLogHandler = new JsLogHandler()
    mainLogger.addHandler(jsLogHandler)
    jsLogHandler.publish(record)
    val gHandler = new GenericLogHandler()
    if (gHandler.isSupported) {
      mainLogger.addHandler(gHandler)
      gHandler.publish(record)
    } else if (!SaxonceApi.isLogHandlerExternal) {
      val loggingWidget = new LoggingPopup()
      val hw = new HasWidgetsLogHandler(loggingWidget)
      mainLogger.addHandler(hw)
      hw.publish(record)
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce

import java.util.logging.{Level, LogRecord, Logger}

import client.net.sf.saxon.ce.lib.TraceListener

import scala.beans.BeanProperty

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

  def initLogger(): Unit = {
//    mainLogger = Logger.getLogger("")
//    val logLevel = Window.Location.getParameter("logLevel")
//    initLogLevel = if ((logLevel == null)) null else Level.parse(logLevel)
    ???
  }

  def LoggingIsDisabledByURI(): Boolean = mainLogger.getLevel == Level.OFF

  @BeanProperty
  var traceListener: TraceListener = null

  def InitializeTraceListener(): Unit = {
    checkTraceIsEnabled()
    if (isTraceEnabled) {
      ???
//      traceListener = new XSLTTraceListener()
    }
  }

  def openTraceListener(): Unit = {
    if (isTraceEnabled) {
      traceListener.open()
    }
  }

  def closeTraceListener(success: Boolean): Unit = {
    if (traceListener != null) {
      if (success) {
        if (isTraceEnabled) {
          traceListener.close()
        }
      } else {
        ???
//        traceListener.asInstanceOf[XSLTTraceListener].terminate()
      }
    }
  }

  def traceIsEnabled(): Boolean = isTraceEnabled

  private def checkTraceIsEnabled(): Boolean = {
    isTraceEnabled = mainLogger.getLevel == Level.FINEST
    isTraceEnabled
  }

//  def addJavaScriptLogHandler() {
//    if (!LoggingIsDisabledByURI()) {
//      Logger.getLogger("").addHandler(new ListenerLogHandler())
//    }
//  }

//  @BeanProperty
//  var jsLogHandler: JsLogHandler = null

  def setLogLevel(newLevel: String): Unit = {
    if (initLogLevel == null) {
      try {
        mainLogger.setLevel(Level.parse(newLevel))
      } catch {
        case e: Exception ⇒ Logger.getLogger("LogController").severe("invalid level for setLogLevel: " + newLevel)
      }
    }
  }

  def getLogLevel(): String = mainLogger.getLevel.getName

  def addRequiredLogHanders(record: LogRecord): Unit = {
    ???
//    jsLogHandler = new JsLogHandler()
//    mainLogger.addHandler(jsLogHandler)
//    jsLogHandler.publish(record)
//    val gHandler = new GenericLogHandler()
//    if (gHandler.isSupported) {
//      mainLogger.addHandler(gHandler)
//      gHandler.publish(record)
//    } else if (!SaxonceApi.isLogHandlerExternal) {
//      val loggingWidget = new LoggingPopup()
//      val hw = new HasWidgetsLogHandler(loggingWidget)
//      mainLogger.addHandler(hw)
//      hw.publish(record)
//    }
  }
}

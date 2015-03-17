package client.net.sf.saxon.ce.lib

import java.util.logging.Handler
import java.util.logging.Level
import java.util.logging.LogRecord
import client.net.sf.saxon.ce.SaxonceApi
import client.net.sf.saxon.ce.js.JSObjectValue
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.core.client.ScriptInjector
import com.google.gwt.logging.client.TextLogFormatter
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Handler that raises JavaScript log events to the host environment
 * Modelled on the GWT Console Logger
 *
 * Sample client-side JavaScript required to handle log events:
 *
 
 // ------------ This is the error handler --------------------
 function handleIxslError() {
 var e = ixslLogData;
 alert(e.details + "\n\n" + e.type);
 }
 
 // ------ this adds a listener for the IXSL Event -----------
 if (window.addEventListener) {
 window.addEventListener("ixslLogEvent", handleIxslError, false);
 } else if (window.attachEvent) {
 window.attachEvent("onixslLogEvent", handleIxslError);
 } else if (window.onLoad) {
 window.onixslLogEvent = handleIxslError;
 }
 -------------------------------------------------------------
 *
 *   window.addEventListener("ixslError", handleIxslError, false);
 */
class JsLogHandler extends Handler {

  setFormatter(new SaxonLogFormatter(true))

  setLevel(Level.ALL)

  override def close() {
  }

  override def flush() {
  }

  override def publish(record: LogRecord) {
    if (!isLoggable(record)) {
      return
    }
    val msg = getFormatter.format(record)
    SaxonceApi.makeCallback(msg, record.getLevel.getName, String.valueOf(record.getMillis))
  }
}

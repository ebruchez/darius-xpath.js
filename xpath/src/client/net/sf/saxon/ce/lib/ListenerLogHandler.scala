package client.net.sf.saxon.ce.lib

import java.util.logging.Handler
import java.util.logging.Level
import java.util.logging.LogRecord
import java.util.logging.Logger
import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.SaxonceApi
//remove if not needed
import scala.collection.JavaConversions._

class ListenerLogHandler extends Handler {

  override def close() {
  }

  override def flush() {
  }

  private var removed: Boolean = false

  override def publish(record: LogRecord) {
    val logger = Logger.getLogger("")
    logger.removeHandler(this)
    val handlerIndex = logger.getHandlers.length
    LogController.addRequiredLogHanders(record)
    val localLogger = Logger.getLogger("ListenerLogHandler")
    localLogger.log(Level.FINE, "Log handlers added (" + 
      (if (SaxonceApi.isLogHandlerExternal) "includes external)" else "internal only)"))
  }
}

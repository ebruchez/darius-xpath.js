package client.net.sf.saxon.ce.lib

import java.util.logging.Handler
import java.util.logging.Level
import java.util.logging.LogRecord
import java.lang.RuntimeException
import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.SaxonceApi
import com.google.gwt.logging.client.TextLogFormatter

/**
 * A Handler that prints logs to the window.console - this is modelled on
 * the gwt firebug handler but designed for other consoles - for firebug,
 * the debug() method is called instead of the log() method
 */
object GenericLogHandler {
  private var useHandler: Boolean = false
  private var isFirebug: Boolean = false
  private var isDirxml: Boolean = false
  private var finestSb: StringBuilder = null

  def aggFinest(str: String) {
    if (finestSb == null) {
      finestSb = new StringBuilder
    }
    finestSb.append(str + "\n")
  }

  def dumpTrace {
    if (LogController.traceIsEnabled) {
      if (finestSb != null) {
        if (isDirxml) {
          logDirxml(finestSb.toString)
        }
        else {
          log(finestSb.toString)
        }
        finestSb = new StringBuilder
      }
    }
  }

  @native
  def logDirxml(text: String)

  @native
  def isDirxml: Boolean

  @native
  private def log(message: String)
}

class GenericLogHandler extends Handler {
  def this() {
    this()
    setFormatter(new SaxonLogFormatter(false))
    setLevel(Level.ALL)
    GenericLogHandler.isFirebug = isFirebug
    GenericLogHandler.isDirxml = GenericLogHandler.isDirxml
    GenericLogHandler.useHandler = (isSupported)
  }

  def close {
  }

  def flush {
  }

  private var previousMillis: Long = 0

  def publish(record: LogRecord) {
    if (record == null) {
      return
    }
    if (!isLoggable(record)) {
      return
    }
    val `val`: Int = record.getLevel.intValue
    val msgText: String = record.getMessage
    if (`val` == Level.SEVERE.intValue) {
      if (previousMillis == record.getMillis || (previousMillis != 0 && msgText.startsWith("[js] "))) {
        return
      }
      previousMillis = record.getMillis
    }
    if (`val` == Level.INFO.intValue || `val` == Level.FINE.intValue) {
      timeMark(msgText)
    }
    if (`val` == Level.FINEST.intValue) {
      GenericLogHandler.aggFinest(msgText)
    }
    else {
      val msg: String = getFormatter.format(record)
      if (`val` <= Level.FINE.intValue) {
        logGeneral(msg)
      }
      else if (`val` < Level.WARNING.intValue) {
        info(msg)
      }
      else if (`val` < Level.SEVERE.intValue) {
        warn(msg)
      }
      else {
        if (LogController.traceIsEnabled && GenericLogHandler.isDirxml && GenericLogHandler.finestSb != null) {
          if (GenericLogHandler.finestSb.length > 0) {
            val m: String = GenericLogHandler.finestSb.toString
            GenericLogHandler.finestSb = new StringBuilder
            logGeneral(m)
          }
        }
        error(msg)
      }
    }
  }

  private def logGeneral(msg: String) {
    if (GenericLogHandler.isFirebug) {
      debug(msg)
    }
    else {
      GenericLogHandler.log(msg)
    }
  }

  @native
  def isSupported: Boolean

  @native
  def isFirebug: Boolean

  @native
  private def debug(message: String)

  @native
  private def timeMark(message: String): Boolean

  @native
  private def error(message: String)

  @native
  private def info(message: String)

  @native
  private def warn(message: String)
}
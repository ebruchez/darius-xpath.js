package client.net.sf.saxon.ce.lib

import com.google.gwt.i18n.client.DateTimeFormat
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat
import com.google.gwt.logging.impl.FormatterImpl
import java.util.Date
import java.util.logging.LogRecord
import SaxonLogFormatter._
//remove if not needed
import scala.collection.JavaConversions._

object SaxonLogFormatter {

  private var dtf: DateTimeFormat = DateTimeFormat.getFormat("HH:mm:ss.SSS")

  private def formatEvent(event: LogRecord): String = {
    val date = new Date(event.getMillis)
    val timeString = dtf.format(date)
    val s = new StringBuilder()
    s.append("SaxonCE.")
    s.append(event.getLoggerName)
    s.append(" ")
    s.append(timeString)
    s.append("\n")
    s.append(event.getLevel.getName)
    s.append(": ")
    s.toString
  }
}

/**
 * Formats LogRecords into 2 lines of text.
 */
class SaxonLogFormatter(var showStackTraces: Boolean) extends FormatterImpl {

  override def format(event: LogRecord): String = {
    val message = new StringBuilder()
    message.append(formatEvent(event))
    message.append(event.getMessage)
    if (showStackTraces) {
      message.append(getStackTraceAsString(event.getThrown, "\n", "\t"))
    }
    message.toString
  }
}

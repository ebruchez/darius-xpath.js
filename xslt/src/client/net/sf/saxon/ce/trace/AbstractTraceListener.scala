package client.net.sf.saxon.ce.trace

import client.net.sf.saxon.ce.Version
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.parser.CodeInjector
import client.net.sf.saxon.ce.lib.GenericLogHandler
import client.net.sf.saxon.ce.lib.StandardErrorListener
import client.net.sf.saxon.ce.lib.TraceListener
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.value.Whitespace
import java.util.Iterator
import java.util.logging.Logger
import AbstractTraceListener._
//remove if not needed
import scala.collection.JavaConversions._

object AbstractTraceListener {

  private var spaceBuffer: StringBuffer = new StringBuffer("                ")

  private var prevModule: String = ""

  /**
   * Get n spaces
   */
  private def spaces(n: Int): String = {
    while (spaceBuffer.length < n) {
      spaceBuffer.append(AbstractTraceListener.spaceBuffer)
    }
    spaceBuffer.substring(0, n)
  }
}

/**
 * This is the standard trace listener equivalent to that used when the -T option is specified on the command line.
 * There are two variants, represented by subclasses: one for XSLT, and one for XQuery. The two variants
 * differ in that they present the trace output in terms of constructs used in the relevant host language.
 */
abstract class AbstractTraceListener extends TraceListener {

  private var indent: Int = 0

  private var logger: Logger = Logger.getLogger("Trace")

  /**
   * Get the associated CodeInjector to be used at compile time to generate the tracing calls
   */
  def getCodeInjector(): CodeInjector = new TraceCodeInjector()

  private var t_total: Long = _

  /**
   * Called at start
   */
  def open() {
    prevModule = ""
    t_total = System.currentTimeMillis()
    logger.finest("<trace " + "saxon-version=\"" + Version.getProductVersion + 
      "\" " + 
      getOpeningAttributes + 
      '>')
    indent += 1
  }

  protected def getOpeningAttributes(): String

  /**
   * Called at end
   */
  def close() {
    t_total = t_total - System.currentTimeMillis()
    indent -= 1
    logger.finest("</trace>")
    GenericLogHandler.dumpTrace()
  }

  def terminate() {
    indent = 0
  }

  /**
   * Called when an instruction in the stylesheet gets processed
   */
  def enterChooseItem(test: String) {
    if (test.isEmpty) {
      logger.finest(AbstractTraceListener.spaces(indent) + "<xsl:otherwise>")
    } else {
      logger.finest(AbstractTraceListener.spaces(indent) + "<xsl:when test=\"" + 
        escape(test) + 
        "\">")
    }
    indent += 1
  }

  def leaveChooseItem(test: String) {
    if (test.isEmpty) {
      logger.finest(AbstractTraceListener.spaces(indent) + "</xsl:otherwise>")
    } else {
      logger.finest(AbstractTraceListener.spaces(indent) + "</xsl:when>")
    }
    indent -= 1
  }

  def enter(info: InstructionInfo, context: XPathContext) {
    val infotype = info.getConstructType
    val qName = info.getObjectName
    val tag = tag(infotype)
    if (tag == null) {
      return
    }
    val file = StandardErrorListener.abbreviatePath(info.getSystemId)
    var msg = AbstractTraceListener.spaces(indent) + '<' + tag
    val name = info.getProperty("name").asInstanceOf[String]
    if (name != null) {
      msg += " name=\"" + escape(name) + '"'
    } else if (qName != null) {
      msg += " name=\"" + escape(qName.getDisplayName) + '"'
    }
    val props = info.getProperties
    while (props.hasNext) {
      var prop = props.next().asInstanceOf[String]
      val `val` = info.getProperty(prop)
      if (prop.startsWith("{")) {
        val rcurly = prop.indexOf('}')
        if (rcurly > 0) {
          prop = prop.substring(rcurly + 1)
        }
      }
      if (`val` != null && prop != "name" && prop != "expression") {
        msg += ' ' + prop + "=\"" + escape(`val`.toString) + '"'
      }
    }
    val newModule = escape(file)
    if (newModule != prevModule) {
      prevModule = newModule
      msg += " module=\"" + newModule + "\">"
    } else {
      msg += ">"
    }
    logger.finest(msg)
    indent += 1
  }

  /**
   * Escape a string for XML output (in an attribute delimited by double quotes).
   * This method also collapses whitespace (since the value may be an XPath expression that
   * was originally written over several lines).
   */
  def escape(in: String): String = {
    if (in == null) {
      return ""
    }
    val collapsed = Whitespace.collapseWhitespace(in)
    val sb = new FastStringBuffer(collapsed.length + 10)
    for (i <- 0 until collapsed.length) {
      val c = collapsed.charAt(i)
      if (c == '<') {
        sb.append("&lt;")
      } else if (c == '>') {
        sb.append("&gt;")
      } else if (c == '&') {
        sb.append("&amp;")
      } else if (c == '\"') {
        sb.append("&#34;")
      } else if (c == '\n') {
        sb.append("&#xA;")
      } else if (c == '\r') {
        sb.append("&#xD;")
      } else if (c == '\t') {
        sb.append("&#x9;")
      } else {
        sb.append(c)
      }
    }
    sb.toString
  }

  /**
   * Called after an instruction of the stylesheet got processed
   */
  def leave(info: InstructionInfo) {
    val infotype = info.getConstructType
    val tag = tag(infotype)
    if (tag == null) {
      return
    }
    indent -= 1
    logger.finest(AbstractTraceListener.spaces(indent) + "</" + tag + '>')
  }

  protected def tag(construct: StructuredQName): String

  /**
   * Called when an item becomes the context item
   */
  def startCurrentItem(item: Item) {
    if (item.isInstanceOf[NodeInfo]) {
      val curr = item.asInstanceOf[NodeInfo]
      logger.finest(AbstractTraceListener.spaces(indent) + "<source node=\"" + 
        Navigator.getPath(curr) + 
        "\" file=\"" + 
        escape(StandardErrorListener.abbreviatePath(curr.getSystemId)) + 
        "\">")
    }
    indent += 1
  }

  /**
   * Called after a node of the source tree got processed
   */
  def endCurrentItem(item: Item) {
    indent -= 1
    if (item.isInstanceOf[NodeInfo]) {
      val curr = item.asInstanceOf[NodeInfo]
      logger.finest(AbstractTraceListener.spaces(indent) + "</source><!-- " + 
        Navigator.getPath(curr) + 
        " -->")
    }
  }
}

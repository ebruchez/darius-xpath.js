package client.net.sf.saxon.ce.trace

import client.net.sf.saxon.ce.expr.parser.CodeInjector
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import XSLTTraceListener._
//remove if not needed
import scala.collection.JavaConversions._

object XSLTTraceListener {

  def tagName(construct: StructuredQName): String = {
    if (construct == Location.LITERAL_RESULT_ELEMENT) {
      "LRE"
    } else if (construct == Location.LITERAL_RESULT_ATTRIBUTE) {
      "ATTR"
    } else if (construct == Location.LET_EXPRESSION) {
      "xsl:variable"
    } else if (construct == Location.TRACE_CALL) {
      "user-trace"
    } else {
      construct.getDisplayName
    }
  }
}

/**
 * A Simple trace listener for XSLT that writes messages (by default) to the developer console
 */
class XSLTTraceListener extends AbstractTraceListener {

  def getCodeInjector(): CodeInjector = new XSLTTraceCodeInjector()

  /**
   * Generate attributes to be included in the opening trace element
   */
  protected def getOpeningAttributes(): String = {
    "xmlns:xsl=\"" + NamespaceConstant.XSLT + '\"' + " xmlns:ixsl=\"" + 
      NamespaceConstant.IXSL + 
      '\"'
  }

  protected def tag(construct: StructuredQName): String = tagName(construct)
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
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

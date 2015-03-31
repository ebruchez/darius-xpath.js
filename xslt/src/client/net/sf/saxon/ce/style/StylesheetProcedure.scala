// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This interface is implemented by all XSL elements that can contain local variable declarations.
 * Specifically, a top-level xsl:template, xsl:variable, xsl:param, or xsl:function element
 * or an xsl:attribute-set element or xsl:key element.
 */
trait StylesheetProcedure {

  /**
   * Optimize the stylesheet construct
   * @param declaration
   */
  def optimize(declaration: Declaration): Unit
}

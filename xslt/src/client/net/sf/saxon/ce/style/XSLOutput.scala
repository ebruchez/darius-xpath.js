// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:output element in the stylesheet.
 */
class XSLOutput extends StyleElement {

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes(): Unit = {
  }

  def validate(decl: Declaration): Unit = {
    checkTopLevel(null)
    checkEmpty()
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:matching-substring and xsl:non-matching-substring elements in stylesheet.
 * New at XSLT 2.0<BR>
 */
class XSLMatchingSubstring extends StyleElement {

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  def prepareAttributes(): Unit = {
    checkForUnknownAttributes()
  }

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def validate(decl: Declaration): Unit = {
    if (!(getParent.isInstanceOf[XSLAnalyzeString])) {
      compileError(getDisplayName + " must be immediately within xsl:analyze-string", "XTSE0010")
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    throw new UnsupportedOperationException("XSLMatchingSubstring#compile() should not be called")
  }
}

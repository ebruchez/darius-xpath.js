// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This element is a surrogate for an extension element (or indeed an xsl element)
 * for which no implementation is available.
 */
class AbsentExtensionElement extends StyleElement {

  def isInstruction(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  /**
   * Process the attributes of this element and all its children
   */
  def processAllAttributes() {
    if (isTopLevel && forwardsCompatibleModeIsEnabled()) {
    } else {
      super.processAllAttributes()
    }
  }

  def prepareAttributes() {
  }

  /**
   * Recursive walk through the stylesheet to validate all nodes
   * @param decl
   */
  def validateSubtree(decl: Declaration) {
    if (isTopLevel && forwardsCompatibleModeIsEnabled()) {
    } else {
      super.validateSubtree(decl)
    }
  }

  def validate(decl: Declaration) {
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    if (isTopLevel) {
      return null
    }
    if (validationError == null) {
      validationError = new XPathException("Unknown instruction")
    }
    fallbackProcessing(exec, decl, this)
  }
}

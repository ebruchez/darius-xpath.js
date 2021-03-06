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
  def processAllAttributes(): Unit = {
    if (isTopLevel && forwardsCompatibleModeIsEnabled()) {
    } else {
      super.processAllAttributes()
    }
  }

  def prepareAttributes(): Unit = {
  }

  /**
   * Recursive walk through the stylesheet to validate all nodes
   * @param decl
   */
  def validateSubtree(decl: Declaration): Unit = {
    if (isTopLevel && forwardsCompatibleModeIsEnabled()) {
    } else {
      super.validateSubtree(decl)
    }
  }

  def validate(decl: Declaration): Unit = {
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

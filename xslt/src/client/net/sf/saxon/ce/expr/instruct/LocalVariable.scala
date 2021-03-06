// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Sequence
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for local xsl:variable elements in stylesheet. Not used in XQuery. In fact, the class is used
 * only transiently in XSLT: local variables are compiled first to a LocalVariable object, and subsequently
 * to a LetExpression.
 */
class LocalVariable extends GeneralVariable {

  /**
   * Process the local variable declaration
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    throw new UnsupportedOperationException("LocalVariable")
  }

  /**
   * Evaluate the variable
   */
  def evaluateVariable(c: XPathContext): Sequence = {
    throw new UnsupportedOperationException("LocalVariable")
  }
}

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
 * An xsl:output-character element in the stylesheet. <br>
 */
class XSLOutputCharacter extends StyleElement {

  def prepareAttributes(): Unit = {
    checkAttribute("character", "s1")
    checkAttribute("string", "s1")
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration): Unit = {
    checkEmpty()
    if (!getParent.isInstanceOf[XSLCharacterMap]) {
      compileError("xsl:output-character may appear only as a child of xsl:character-map", "XTSE0010")
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}

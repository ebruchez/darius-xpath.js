// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.instruct.AttributeCreator
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.SetAttribute
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Extension element for Saxon client edition: ixsl:set-attribute
 */
class IXSLSetAttribute extends XSLAttribute {

  override def compile(exec: Executable, decl: Declaration): Expression = {
    val constructor = super.compile(exec, decl)
    new SetAttribute(constructor.asInstanceOf[AttributeCreator], SetAttribute.SET)
  }
}

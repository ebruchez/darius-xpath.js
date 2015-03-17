// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.AttributeCreator
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.SetAttribute
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Extension element for Saxon client edition: ixsl:remove-attribute
 */
class IXSLRemoveAttribute extends XSLAttribute {

  override def prepareAttributes() {
    super.prepareAttributes()
    if (select != null) {
      compileError("ixsl:remove-attribute does not allow @select")
    }
  }

  override def validate(decl: Declaration) {
    super.validate(decl)
    checkEmpty()
  }

  override def compile(exec: Executable, decl: Declaration): Expression = {
    val constructor = super.compile(exec, decl)
    new SetAttribute(constructor.asInstanceOf[AttributeCreator], SetAttribute.REMOVE)
  }
}

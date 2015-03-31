// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.WithParam
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:with-param element in the stylesheet. <br>
 * The xsl:with-param element has mandatory attribute name and optional attribute select
 */
class XSLWithParam extends XSLGeneralVariable {

  protected def allowsAsAttribute(): Boolean = true

  protected def allowsTunnelAttribute(): Boolean = true

  def validate(decl: Declaration): Unit = {
    super.validate(decl)
    val iter = iterateAxis(Axis.PRECEDING_SIBLING, NodeKindTest.ELEMENT)
    while (true) {
      val prev = iter.next()
      if (prev == null) {
        //break
      }
      if (prev.isInstanceOf[XSLWithParam]) {
        if (this.getVariableQName == prev.asInstanceOf[XSLWithParam].getVariableQName) {
          compileError("Duplicate parameter name", "XTSE0670")
        }
      }
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val psm = getPrincipalStylesheetModule
    val inst = new WithParam()
    inst.adoptChildExpression(select)
    inst.setParameterId(psm.allocateUniqueParameterNumber(getVariableQName))
    initializeInstruction(exec, decl, inst)
    inst
  }
}

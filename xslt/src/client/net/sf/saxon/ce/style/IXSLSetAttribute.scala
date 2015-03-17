package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.AttributeCreator
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.SetAttribute
import client.net.sf.saxon.ce.trans.XPathException
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

package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.StructuredQName
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Generic interface representing a variable declaration in the static context of an XPath expression.
 * The declaration may be internal or external to the XPath expression itself. An external
 * VariableDeclaration is identified (perhaps created) by the bindVariable() method in the StaticContext.
 */
trait VariableDeclaration {

  /**
   * Method called by a BindingReference to register the variable reference for
   * subsequent fixup.
   * This method is called by the XPath parser when
   * each reference to the variable is encountered. At some time after parsing and before execution of the
   * expression, the VariableDeclaration is responsible for calling the two methods setStaticType()
   * and fixup() on each BindingReference that has been registered with it.<br>
   * @param ref the variable reference
   */
  def registerReference(ref: VariableReference): Unit

  /**
   * Get the name of the variable as a structured QName
   * @return the variable name
   */
  def getVariableQName(): StructuredQName
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.`type`.AtomicType
import org.orbeon.darius.xpath.expr.{CastExpression, Container, Expression, StaticContext}
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException

object ConstructorFunctionLibrary {

  val THE_INSTANCE: ConstructorFunctionLibrary = new ConstructorFunctionLibrary()

  def getInstance: ConstructorFunctionLibrary = THE_INSTANCE
}

/**
 * The ConstructorFunctionLibrary represents the collection of constructor functions for atomic types. These
 * are provided for the built-in types such as xs:integer and xs:date, and also for user-defined atomic types.
 */
class ConstructorFunctionLibrary private () extends FunctionLibrary {

  /**
   * Test whether a system function with a given name and arity is available. This supports
   * the function-available() function in XSLT. This method may be called either at compile time
   * or at run time.
   * @param functionName the name of the function
   * @param arity The number of arguments. This is set to -1 in the case of the single-argument
   * @return if a function of this name and arity is available for calling, then the type signature of the
   * function, as an array of sequence types in which the zeroth entry represents the return type; otherwise null
   */
  def hasFunctionSignature(functionName: StructuredQName, arity: Int): Boolean = {
    if (arity != 1 && arity != -1) {
      return false
    }
    val uri = functionName.getNamespaceURI
    val local = functionName.getLocalName
    uri == NamespaceConstant.SCHEMA && AtomicType.getSchemaType(local) != null
  }

  /**
   * Bind an extension function, given the URI and local parts of the function name,
   * and the list of expressions supplied as arguments. This method is called at compile
   * time.
   * @param functionName
   * @param arguments  The expressions supplied statically in the function call. The intention is
   * that the static type of the arguments (obtainable via getItemType() and getCardinality() may
   * be used as part of the binding algorithm.
   * @param env
   * @param container
   * @return An object representing the extension function to be called, if one is found;
   * null if no extension function was found matching the required name and arity.
   * @throws org.orbeon.darius.xpath.trans.XPathException if a function is found with the required name and arity, but
   * the implementation of the function cannot be loaded or used; or if an error occurs
   * while searching for the function; or if this function library "owns" the namespace containing
   * the function call, but no function was found.
   */
  def bind(functionName: StructuredQName, 
      arguments: Array[Expression], 
      env: StaticContext, 
      container: Container): Expression = {
    val uri = functionName.getNamespaceURI
    val localName = functionName.getLocalName
    if (uri == NamespaceConstant.SCHEMA) {
      if (arguments.length != 1) {
        throw new XPathException("A constructor function must have exactly one argument")
      }
      val `type` = AtomicType.getSchemaType(localName)
      if (`type` == null || `type` == AtomicType.ANY_ATOMIC) {
        val err = new XPathException("Unknown constructor function: {" + uri + '}' + localName, "XPST0017")
        err.setIsStaticError(true)
        throw err
      }
      val cast = new CastExpression(arguments(0), `type`.asInstanceOf[AtomicType], true)
      cast.setContainer(container)
      return cast
    }
    null
  }
}

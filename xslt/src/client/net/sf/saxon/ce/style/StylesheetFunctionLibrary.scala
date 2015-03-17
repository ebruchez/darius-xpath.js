package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Container
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.StaticContext
import client.net.sf.saxon.ce.expr.UserFunctionCall
import client.net.sf.saxon.ce.functions.FunctionLibrary
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A StylesheetFunctionLibrary contains functions defined by the user in a stylesheet. This library is used at
 * compile time only, as it contains references to the actual XSLFunction objects. Binding to a function in this
 * library registers the function call on a fix-up list to be notified when the actual compiled function becomes
 * available.
 */
class StylesheetFunctionLibrary(sheet: PrincipalStylesheetModule, @BooleanBeanProperty var overriding: Boolean)
    extends FunctionLibrary {

  private var stylesheet: PrincipalStylesheetModule = sheet

  /**
   * Test whether a function with a given name and arity is available; if so, return its signature.
   * This supports the function-available() function in XSLT; it is also used to support
   * higher-order functions introduced in XQuery 1.1.
   *
   * <p>This method may be called either at compile time
   * or at run time. If the function library is to be used only in an XQuery or free-standing XPath
   * environment, this method may throw an UnsupportedOperationException.</p>
   * @param functionName the qualified name of the function being called
   * @param arity        The number of arguments. This is set to -1 in the case of the single-argument
   *                     function-available() function; in this case the method should return true if there is some
   *                     function of this name available for calling.
   * @return if a function of this name and arity is available for calling, then the type signature of the
   * function, as an array of sequence types in which the zeroth entry represents the return type; or a zero-length
   * array if the function exists but the signature is not known; or null if the function does not exist
   */
  def hasFunctionSignature(functionName: StructuredQName, arity: Int): Boolean = {
    stylesheet.getFunction(functionName, arity) != null
  }

  /**
   * Bind a function, given the URI and local parts of the function name,
   * and the list of expressions supplied as arguments. This method is called at compile
   * time.
   * @param functionName
   * @param staticArgs  The expressions supplied statically in the function call. The intention is
   * that the static type of the arguments (obtainable via getItemType() and getCardinality() may
   * be used as part of the binding algorithm.
   * @param env
   * @param container
   * @return An object representing the extension function to be called, if one is found;
   * null if no extension function was found matching the required name and arity.
   * @throws client.net.sf.saxon.ce.trans.XPathException if a function is found with the required name and arity, but
   * the implementation of the function cannot be loaded or used; or if an error occurs
   * while searching for the function; or if this function library "owns" the namespace containing
   * the function call, but no function was found.
   */
  def bind(functionName: StructuredQName, 
      staticArgs: Array[Expression], 
      env: StaticContext, 
      container: Container): Expression = {
    val fn = stylesheet.getFunction(functionName, staticArgs.length)
    if (fn == null) {
      return null
    }
    if (fn.isOverriding != overriding) {
      return null
    }
    val fc = new UserFunctionCall()
    fn.registerReference(fc)
    fc.setFunctionName(functionName)
    fc.setArguments(staticArgs)
    fc.setContainer(container)
    fc
  }
}

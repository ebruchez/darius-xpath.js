// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.{Container, Expression, StaticContext}
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.orbeon.{ArrayList, List}

/**
 * A FunctionLibraryList is a list of FunctionLibraries. It is also a FunctionLibrary in its own right.
 * When required, it searches the list of FunctionLibraries to find the required function.
 */
class FunctionLibraryList extends FunctionLibrary {

  var libraryList: List[FunctionLibrary] = new ArrayList(8)

  /**
   * Add a new FunctionLibrary to the list of FunctionLibraries in this FunctionLibraryList. Note
   * that libraries are searched in the order they are added to the list.
   * @param lib A function library to be added to the list of function libraries to be searched.
   * @return the position of the library in the list
   */
  def addFunctionLibrary(lib: FunctionLibrary): Int = {
    libraryList.add(lib)
    libraryList.size - 1
  }

  /**
   * Get the n'th function library in the list
   */
  def get(n: Int): FunctionLibrary = libraryList.get(n)

  /**
   * Test whether a system function with a given name and arity is available, and return its signature. This supports
   * the function-available() function in XSLT. This method may be called either at compile time
   * or at run time.
   * @param functionName the name of the function
   * @param arity The number of arguments. This is set to -1 in the case of the single-argument function-available() function
   * @return if a function of this name and arity is available for calling, then the type signature of the
   * function, as an array of sequence types in which the zeroth entry represents the return type; otherwise null
   */
  def hasFunctionSignature(functionName: StructuredQName, arity: Int): Boolean = {
    val it = libraryList.iterator()
    while (it.hasNext) {
      val lib = it.next()
      val b = lib.hasFunctionSignature(functionName, arity)
      if (b) {
        return true
      }
    }
    false
  }

  /**
   * Bind an extension function, given the URI and local parts of the function name,
   * and the list of expressions supplied as arguments. This method is called at compile
   * time.
   * @param functionName
   * @param staticArgs  The expressions supplied statically in arguments to the function call.
   * The length of this array represents the arity of the function. The intention is
   * that the static type of the arguments (obtainable via getItemType() and getCardinality() may
   * be used as part of the binding algorithm. In some cases it may be possible for the function
   * to be pre-evaluated at compile time, for example if these expressions are all constant values.
   * @param env
   * @param container
   * @return An object representing the extension function to be called, if one is found;
   * null if no extension function was found matching the required name and arity.
   * @throws client.net.sf.saxon.ce.trans.XPathException if a function is found with the required name and arity, but
   * the implementation of the function cannot be loaded or used; or if an error occurs
   * while searching for the function.
   */
  def bind(functionName: StructuredQName, 
      staticArgs: Array[Expression], 
      env: StaticContext, 
      container: Container): Expression = {
    for (lib <- libraryList) {
      val func = lib.bind(functionName, staticArgs, env, container)
      if (func != null) {
        return func
      }
    }
    null
  }
}

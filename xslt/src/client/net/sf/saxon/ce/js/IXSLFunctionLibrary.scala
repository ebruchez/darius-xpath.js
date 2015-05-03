// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.js

import org.orbeon.darius.xpath.expr.Container
import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.StaticContext
import org.orbeon.darius.xpath.expr.StringLiteral
import org.orbeon.darius.xpath.value.StringValue
import org.orbeon.darius.xpath.functions.FunctionLibrary
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
import IXSLFunctionLibrary._
//remove if not needed
import scala.collection.JavaConversions._

object IXSLFunctionLibrary {

  private /* native */ def exists(member: String): Boolean
}

/**
 * Library of Saxon-defined extension functions for the browser environment
 */
class IXSLFunctionLibrary extends FunctionLibrary {

  def hasFunctionSignature(functionName: StructuredQName, arity: Int): Boolean = {
    val uri = functionName.getNamespaceURI
    if (NamespaceConstant.IXSL == uri) {
      return true
    } else if (NamespaceConstant.JS == uri) {
      return exists(functionName.getLocalName)
    }
    false
  }

  def bind(functionName: StructuredQName, 
      staticArgs: Array[Expression], 
      env: StaticContext, 
      container: Container): Expression = {
    val uri = functionName.getNamespaceURI
    val local = functionName.getLocalName
    if (NamespaceConstant.IXSL == uri) {
      if (!hasFunctionSignature(functionName, staticArgs.length)) {
        return null
      }
      new IXSLFunction(local, staticArgs)
    } else if (NamespaceConstant.JS == uri) {
      val args = Array.ofDim[Expression](staticArgs.length + 2)
      System.arraycopy(staticArgs, 0, args, 2, staticArgs.length)
      args(0) = new IXSLFunction("window", Array.ofDim[Expression](0))
      args(1) = StringLiteral.makeLiteral(new StringValue(local))
      new IXSLFunction("call", args)
    } else if (NamespaceConstant.EXSLT_COMMON == uri && local == "node-set" && 
      staticArgs.length == 1) {
      staticArgs(0)
    } else {
      null
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.functions.FunctionLibrary
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An ExpressionContext represents the context for an XPath expression written
 * in the stylesheet.
 */
class ExpressionContext(var element: StyleElement) extends StaticContext {

  /**
   * Get the system configuration
   */
  def getConfiguration: Configuration = element.getConfiguration

  /**
   * Get the executable
   * @return the executable
   */
  def getExecutable(): Executable = element.getExecutable

  /**
   * Get the System ID of the entity containing the expression (used for diagnostics)
   */
  def getSystemId: String = element.getSystemId

  /**
   * Get the Base URI of the element containing the expression, for resolving any
   * relative URI's used in the expression.
   * Used by the document() function.
   */
  def getBaseURI: String = element.getBaseURI

  /**
   * Get a copy of the NamespaceResolver suitable for saving in the executable code
   * @return a NamespaceResolver
   */
  def getNamespaceResolver: NamespaceResolver = new InscopeNamespaceResolver(element)

  /**
   * Bind a variable to an object that can be used to refer to it
   * @param qName the name of the variable
   * @return a VariableDeclaration object that can be used to identify it in the Bindery,
   * @throws XPathException if the variable has not been declared
   */
  def bindVariable(qName: StructuredQName): Expression = {
    val xslVariableDeclaration = element.bindVariable(qName)
    if (xslVariableDeclaration == null) {
      val err = new XPathException("Variable " + qName.getDisplayName + " has not been declared")
      err.setErrorCode("XPST0008")
      err.setIsStaticError(true)
      throw err
    }
    val `var` = if (xslVariableDeclaration.isGlobal) new VariableReference() else new LocalVariableReference()
    xslVariableDeclaration.registerReference(`var`)
    `var`
  }

  /**
   * Get the function library containing all the in-scope functions available in this static
   * context
   */
  def getFunctionLibrary: FunctionLibrary = {
    element.getPrincipalStylesheetModule.getFunctionLibrary
  }

  /**
   * Get the default collation. Return null if no default collation has been defined
   */
  def getDefaultCollationName: String = element.getDefaultCollationName

  /**
   * Get the default XPath namespace for elements and types
   * Return NamespaceConstant.NULL for the non-namespace
   */
  def getDefaultElementNamespace: String = element.getDefaultXPathNamespace

  /**
   * Get the default function namespace
   */
  def getDefaultFunctionNamespace: String = NamespaceConstant.FN

  /**
   * Determine whether Backwards Compatible Mode is used
   */
  def isInBackwardsCompatibleMode: Boolean = element.xPath10ModeIsEnabled()

  /**
   * Get the containing element in the stylesheet
   * @return the stylesheet element
   */
  def getStyleElement(): StyleElement = element
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.sxpath

import org.orbeon.darius.xpath.expr.StaticContext
import org.orbeon.darius.xpath.functions.{FunctionLibrary, FunctionLibraryList}
import org.orbeon.darius.xpath.lib.NamespaceConstant
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.trans.DecimalFormatManager

import scala.beans.BeanProperty

/**
 * An abstract and configurable implementation of the StaticContext interface,
 * which defines the static context of an XPath expression.
 *
 * <p>This class implements those parts of the functionality of a static context
 * that tend to be common to most implementations: simple-valued properties such
 * as base URI and default element namespace; availability of the standard
 * function library; and support for collations.</p>
 */
abstract class AbstractStaticContext extends StaticContext {

  private var baseURI: String = null

  private var config: Configuration = _

  private var libraryList: FunctionLibraryList = new FunctionLibraryList()

  @BeanProperty
  var defaultFunctionNamespace: String = NamespaceConstant.FN

  @BeanProperty
  var defaultElementNamespace: String = NamespaceConstant.NULL

  private var decimalFormatManager: DecimalFormatManager = null

  private var backwardsCompatible: Boolean = false

  protected var usingDefaultFunctionLibrary: Boolean = _

  /**
   * Set the Configuration.
   * @param config the configuration
   */
  def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }

  /**
   * Get the system configuration
   */
  def getConfiguration: Configuration = config

  /**
   * Set the base URI in the static context
   * @param baseURI the base URI of the expression
   */
  def setBaseURI(baseURI: String): Unit = {
    this.baseURI = baseURI
  }

  /**
   * Get the Base URI, for resolving any relative URI's used
   * in the expression. Used by the document() function, resolve-uri(), etc.
   * @return "" if no base URI has been set
   */
  def getBaseURI: String = if (baseURI == null) "" else baseURI

  /**
   * Get the function library containing all the in-scope functions available in this static
   * context. This method is called by the XPath parser when binding a function call in the
   * XPath expression to an implementation of the function.
   */
  def getFunctionLibrary: FunctionLibrary = libraryList

  /**
   * Set the function library to be used
   * @param lib the function library
   */
  def setFunctionLibrary(lib: FunctionLibraryList): Unit = {
    libraryList = lib
    usingDefaultFunctionLibrary = false
  }

  /**
   * Get the name of the default collation.
   * @return the name of the default collation; or the name of the codepoint collation
   * if no default collation has been defined
   */
  def getDefaultCollationName: String = {
    NamespaceConstant.CODEPOINT_COLLATION_URI
  }

  /**
   * Get the system ID of the container of the expression. Used to construct error messages.
   * @return "" always
   */
  def getSystemId: String = ""

  /**
   * Get the line number of the expression within that container.
   * Used to construct error messages.
   * @return -1 always
   */
  private def getLineNumber: Int = -1

  /**
   * Set XPath 1.0 backwards compatibility mode on or off
   * @param option true if XPath 1.0 compatibility mode is to be set to true;
   * otherwise false
   */
  def setBackwardsCompatibilityMode(option: Boolean): Unit = {
    backwardsCompatible = option
  }

  /**
   * Determine whether Backwards Compatible Mode is used
   * @return true if XPath 1.0 compatibility mode is to be set to true;
   * otherwise false
   */
  def isInBackwardsCompatibleMode: Boolean = backwardsCompatible

  /**
   * Set the DecimalFormatManager used to resolve the names of decimal formats used in calls
   * to the format-number() function.
   * @param manager the decimal format manager for this static context, or null if no named decimal
   *         formats are available in this environment.
   */
  def setDecimalFormatManager(manager: DecimalFormatManager): Unit = {
    this.decimalFormatManager = manager
  }
}

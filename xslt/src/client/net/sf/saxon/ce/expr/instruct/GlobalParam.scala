// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The compiled form of a global xsl:param element in an XSLT stylesheet or an
 * external variable declared in the prolog of a Query. <br>
 * The xsl:param element in XSLT has mandatory attribute name and optional attribute select. It can also
 * be specified as required="yes" or required="no". In standard XQuery 1.0 external variables are always required,
 * and no default value can be specified; but Saxon provides an extension pragma that allows a query
 * to specify a default. XQuery 1.1 adds standard syntax for defining a default value.
 */
class GlobalParam extends GlobalVariable {

  /**
   * Evaluate the variable
   */
  override def evaluateVariable(context: XPathContext): Sequence = {
    val controller = context.getController
    val b = controller.getBindery
    var wasSupplied: Boolean = false
    try {
      wasSupplied = b.useGlobalParameter(getVariableQName, getSlotNumber, getRequiredType, context)
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
    val `val` = b.getGlobalVariable(getSlotNumber)
    if (wasSupplied || `val` != null) {
      `val`
    } else {
      if (isRequiredParam) {
        dynamicError("No value supplied for required global parameter $" + 
          getVariableQName.getDisplayName, "XTDE0050")
      } else if (isImplicitlyRequiredParam) {
        dynamicError("A value must be supplied for global parameter $" + getVariableQName.getDisplayName + 
          " because there is no default value for the required type", "XTDE0610")
      }
      actuallyEvaluate(context)
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.{Sequence, StructuredQName}
import client.net.sf.saxon.ce.value.SequenceType

/**
 * Binding is a interface used to represent the run-time properties and methods
 * associated with a variable: specifically, a method to get the value
 * of the variable.
 */
trait Binding {

  /**
   * Get the declared type of the variable
   * @return the declared type
   */
  def getRequiredType(): SequenceType

  /**
   * Evaluate the variable
   * @param context the XPath dynamic evaluation context
   * @return the result of evaluating the variable
   */
  def evaluateVariable(context: XPathContext): Sequence

  /**
   * Indicate whether the binding is local or global. A global binding is one that has a fixed
   * value for the life of a query or transformation; any other binding is local.
   * @return true if the binding is global
   */
  def isGlobal(): Boolean

  /**
   * If this is a local variable held on the local stack frame, return the corresponding slot number.
   * In other cases, return -1.
   * @return the slot number on the local stack frame
   */
  def getLocalSlotNumber(): Int

  /**
   * Get the name of the variable
   * @return the name of the variable, as a structured QName
   */
  def getVariableQName(): StructuredQName
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException

/**
 * Variable reference: a reference to a local variable. This subclass of VariableReference
 * bypasses the Binding object to get the value directly from the relevant slot in the local
 * stackframe.
 */
/**
 * Create a LocalVariableReference bound to a given Binding
 * @param binding the binding (that is, the declaration of this local variable)
 */
class LocalVariableReference(binding: Binding) extends VariableReference(binding) {

  var slotNumber: Int = -999

//ORBEON unused
//  /**
//   * Create a clone copy of this VariableReference
//   * @return the cloned copy
//   */
//  private def copy(): Expression = {
//    if (binding == null) {
//      throw new UnsupportedOperationException("Cannot copy a variable reference whose binding is unknown")
//    }
//    val ref = new LocalVariableReference()
//    ref.binding = binding
//    ref.staticType = staticType
//    ref.slotNumber = slotNumber
//    ref.constantValue = constantValue
//    ref.displayName = displayName
//    ExpressionTool.copyLocationInfo(this, ref)
//    ref
//  }

  /**
   * Set the slot number for this local variable, that is, its position in the local stack frame
   * @param slotNumber the slot number to be used
   */
  def setSlotNumber(slotNumber: Int) {
    this.slotNumber = slotNumber
  }

  /**
   * Get the slot number allocated to this local variable
   * @return the slot number
   */
  def getSlotNumber(): Int = slotNumber

  /**
   * Return the value of the variable
   * @param c the XPath dynamic context
   * @return the value of the variable
   * @throws XPathException if any dynamic error occurs while evaluating the variable
   */
  override def evaluateVariable(c: XPathContext): Sequence = {
    try {
      c.getStackFrame()(slotNumber)
    } catch {
      case err: ArrayIndexOutOfBoundsException => {
        if (slotNumber == -999) {
          throw new ArrayIndexOutOfBoundsException("Local variable " + getDisplayName + " has not been allocated a stack frame slot")
        }
        throw err
      }
    }
  }
}

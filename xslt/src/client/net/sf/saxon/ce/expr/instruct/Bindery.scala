// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.DocumentPool
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.value.EmptySequence
import client.net.sf.saxon.ce.value.SequenceExtent
import client.net.sf.saxon.ce.value.SequenceType
import java.util.HashMap
import Bindery._
//remove if not needed
import scala.collection.JavaConversions._

object Bindery {

  /**
   * Apply the function conversion rules to a value, given a required type.
   * @param value a value to be converted
   * @param requiredType the required type
   * @param context the conversion context
   * @return the converted value
   * @throws XPathException if the value cannot be converted to the required type
   */
  def applyFunctionConversionRules(qName: StructuredQName, 
      value: Sequence, 
      requiredType: SequenceType, 
      context: XPathContext): Sequence = {
    val role = new RoleLocator(RoleLocator.PARAM, qName, 0)
    val e = TypeChecker.staticTypeCheck(new Literal(value), requiredType, backwardsCompatible = false, role)
    SequenceExtent.makeSequenceExtent(e.iterate(context))
  }
}

/**
 * The Bindery class holds information about variables and their values. From Saxon 8.1, it is
 * used only for global variables: local variables are now held in the XPathContext object.
 *
 * Variables are identified by a Binding object. Values will always be of class Value.
 */
class Bindery {

  private var globals: Array[Sequence] = _

  private var busy: Array[Boolean] = _

  private var globalParameters: HashMap[StructuredQName, Sequence] = _

  /**
   * Define how many slots are needed for global variables
   * @param numberOfGlobals number of slots needed for global variables.
   */
  def allocateGlobals(numberOfGlobals: Int): Unit = {
    val n = numberOfGlobals + 1
    globals = Array.ofDim[Sequence](n)
    busy = Array.ofDim[Boolean](n)
    for (i ← 0 until n) {
      globals(i) = null
      busy(i) = false
    }
  }

  /**
   * Define global parameters
   * @param params The ParameterSet passed in by the user, eg. from the command line
   */
  def defineGlobalParameters(params: HashMap[StructuredQName, Sequence]): Unit = {
    globalParameters = params
  }

  /**
   * Use global parameter. This is called when a global xsl:param element is processed.
   * If a parameter of the relevant name was supplied, it is bound to the xsl:param element.
   * Otherwise the method returns false, so the xsl:param default will be evaluated.
   * @param qName The name of the parameter
   * @param slot The slot number allocated to the parameter
   * @param requiredType The declared type of the parameter
   * @param context the XPath dynamic evaluation context
   * @return true if a parameter of this name was supplied, false if not
   */
  def useGlobalParameter(qName: StructuredQName, 
      slot: Int, 
      requiredType: SequenceType, 
      context: XPathContext): Boolean = {
    if (globals != null && globals(slot) != null) {
      return true
    }
    if (globalParameters == null) {
      return false
    }
    val obj = globalParameters.get(qName)
    if (obj == null) {
      return false
    }
    if (obj.isInstanceOf[DocumentInfo]) {
      val systemId = obj.asInstanceOf[DocumentInfo].getSystemId
      try {
        if (systemId != null && new URI(systemId, true).isAbsolute) {
          val pool = context.getController.getDocumentPool
          if (pool.find(systemId) == null) {
            pool.add(obj.asInstanceOf[DocumentInfo], systemId)
          }
        }
      } catch {
        case err: URI.URISyntaxException ⇒
      }
    }
    var `val`: Sequence = null
    if (obj.isInstanceOf[Sequence]) {
      `val` = obj.asInstanceOf[Sequence]
    }
    if (`val` == null) {
      `val` = EmptySequence.getInstance
    }
    `val` = applyFunctionConversionRules(qName, `val`, requiredType, context)
    val err = TypeChecker.testConformance(`val`.iterate(), requiredType)
    if (err != null) {
      throw new XPathException(err, "XPTY0004")
    }
    globals(slot) = `val`
    true
  }

  /**
   * Set/Unset a flag to indicate that a particular global variable is currently being
   * evaluated. Note that this code is not synchronized, so there is no absolute guarantee that
   * two threads will not both evaluate the same global variable; however, apart from wasted time,
   * it is harmless if they do.
   * @param binding the global variable in question
   * @return true if evaluation of the variable should proceed; false if it is found that the variable has now been
   * evaluated in another thread.
   * @throws client.net.sf.saxon.ce.trans.XPathException If an attempt is made to set the flag when it is already set, this means
   * the definition of the variable is circular.
   */
  def setExecuting(binding: GlobalVariable): Boolean = {
    val slot = binding.getSlotNumber
    if (busy(slot)) {
      throw new XPathException.Circularity("Circular definition of variable " + binding.getVariableQName.getDisplayName)
    }
    busy(slot) = false
    true
  }

  /**
   * Indicate that a global variable is not currently being evaluated
   * @param binding the global variable
   */
  def setNotExecuting(binding: GlobalVariable): Unit = {
    val slot = binding.getSlotNumber
    busy(slot) = false
  }

  /**
   * Save the value of a global variable, and mark evaluation as complete.
   * @param binding the global variable in question
   * @param value the value that this thread has obtained by evaluating the variable
   * @return the value actually given to the variable. Exceptionally this will be different from the supplied
   * value if another thread has evaluated the same global variable concurrently. The returned value should be
   * used in preference, to ensure that all threads agree on the value. They could be different if for example
   * the variable is initialized using the collection() function.
   */
  def saveGlobalVariableValue(binding: GlobalVariable, value: Sequence): Sequence = {
    synchronized {
      val slot = binding.getSlotNumber
      if (globals(slot) != null) {
        globals(slot)
      } else {
        busy(slot) = false
        globals(slot) = value
        value
      }
    }
  }

  /**
   * Get the value of a global variable whose slot number is known
   * @param slot the slot number of the required variable
   * @return the Value of the variable if defined, null otherwise.
   */
  def getGlobalVariable(slot: Int): Sequence = globals(slot)
}

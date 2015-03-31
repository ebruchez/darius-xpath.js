// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import java.util.Arrays

import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr.XPathContext._
import client.net.sf.saxon.ce.expr.instruct.ParameterSet
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.{Configuration, Controller}
import client.net.sf.saxon.ce.trans.{Rule, XPathException}
import client.net.sf.saxon.ce.tree.iter.{FocusIterator, SingletonIterator}

object XPathContext {
  private val EMPTY_STACKFRAME = new Array[Sequence](0)
}

/**
 * This class represents a context in which an XPath expression is evaluated.
 */
class XPathContext(var controller: Controller) {

  var currentIterator: FocusIterator = _

  var currentReceiver: SequenceReceiver = _

  var isTemporaryDestination: Boolean = false

  var caller: XPathContext = null

  protected var stackFrame: Array[Sequence] = _

//ORBEON XSLT
//  @BeanProperty
//  lazy val localParameters = new ParameterSet()
//
//  @BeanProperty
//  var tunnelParameters: ParameterSet = _
//  private var tailCallFunction: UserFunction = _
//  private var currentMode: Mode = _
//  private var currentTemplate: Rule = _
//
//  @BeanProperty
//  var currentGroupIterator: GroupIterator = _
//
//  @BeanProperty
//  var currentRegexIterator: ARegexIterator = _

  /**
   * Construct a new context as a copy of another. The new context is effectively added
   * to the top of a stack, and contains a pointer to the previous context
   * @return a new context, created as a copy of this context
   */
  def newContext(): XPathContext = {
    val c = new XPathContext(controller)
    c.currentIterator = currentIterator
    c.isTemporaryDestination = isTemporaryDestination
    c.stackFrame = stackFrame
//ORBEON XSLT
//    c.localParameters = localParameters
//    c.tunnelParameters = tunnelParameters
    c.currentReceiver = currentReceiver
//ORBEON XSLT
//    c.currentMode = currentMode
//    c.currentTemplate = currentTemplate
//    c.currentRegexIterator = currentRegexIterator
//    c.currentGroupIterator = currentGroupIterator
    c.caller = this
//ORBEON XSLT
//    c.tailCallFunction = null
    c
  }

  /**
   * Construct a new context without copying (used for the context in a function call)
   * @return a new clean context
   */
  def newCleanContext(): XPathContext = {
    val c = new XPathContext(controller)
    c.stackFrame = EMPTY_STACKFRAME
    c.caller = this
    c
  }

  /**
   * Construct a new minor context. A minor context can only hold new values of the focus
   * (currentIterator) and current output destination.
   * @return a new minor context
   */
  def newMinorContext(): XPathContext = newContext()

  /**
   * Set the local and tunnel parameters for the current template call.
   * @param slots the total number of slots needed on the stackframe
   * @param localParameters the supplied non-tunnel parameters
   * @param tunnelParameters the supplied tunnel parameters
   */
  def setParameters(slots: Int, localParameters: ParameterSet, tunnelParameters: ParameterSet): Unit = {
    openStackFrame(slots)
//ORBEON XSLT
//    this.localParameters = localParameters
//    this.tunnelParameters = tunnelParameters
  }

  /**
   * Set the local stack frame. This method is used when creating a Closure to support
   * delayed evaluation of expressions. The "stack frame" is actually on the Java heap, which
   * means it can survive function returns and the like.
   * @param size the number of slots needed on the stack frame
   * @param variables the array of "slots" to hold the actual variable values. This array will be
   * copied if it is too small to hold all the variables defined in the SlotManager
   */
  def setStackFrame(size: Int, variables: Array[Sequence]): Unit = {
    stackFrame = variables
    if (variables.length != size) {
      if (variables.length > size) {
        throw new IllegalStateException("Attempting to set more local variables (" + variables.length + 
          ") than the stackframe can accommodate (" + 
          size + 
          ")")
      }
      stackFrame = new Array[Sequence](size)
      System.arraycopy(variables, 0, stackFrame, 0, variables.length)
    }
  }

  /**
   * Reset the stack frame variable map, while reusing the StackFrame object itself. This
   * is done on a tail call to a different function
   * @param numberOfSlots the number of slots needed for the stack frame contents
   * @param numberOfParams the number of parameters required on the new stack frame
   */
  def resetStackFrameMap(numberOfSlots: Int, numberOfParams: Int): Unit = {
    if (stackFrame.length != numberOfSlots) {
      val v2 = new Array[Sequence](numberOfSlots)
      System.arraycopy(stackFrame, 0, v2, 0, numberOfParams)
      stackFrame = v2
    } else {
      // not strictly necessary
      //ORBEON cast to Array[AnyRef]
      Arrays.fill(stackFrame.asInstanceOf[Array[AnyRef]], numberOfParams, stackFrame.length, null)
    }
  }

//ORBEON XSLT
//  /**
//   * Reset the local stack frame. This method is used when processing a tail-recursive function.
//   * Instead of the function being called recursively, the parameters are set to new values and the
//   * function body is evaluated repeatedly
//   * @param fn the user function being called using tail recursion
//   * @param variables the parameter to be supplied to the user function
//   */
//  def requestTailCall(fn: UserFunction, variables: Array[Sequence]) {
//    if (variables.length > stackFrame.length) {
//      val v2 = new Array[Sequence](fn.getNumberOfSlots)
//      System.arraycopy(variables, 0, v2, 0, variables.length)
//      stackFrame = v2
//    } else {
//      System.arraycopy(variables, 0, stackFrame, 0, variables.length)
//    }
//    tailCallFunction = fn
//  }

//ORBEON XSLT
//  /**
//   * Determine whether the body of a function is to be repeated, due to tail-recursive function calls
//   * @return null if no tail call has been requested, or the name of the function to be called otherwise
//   */
//  def getTailCallFunction(): UserFunction = {
//    val fn = tailCallFunction
//    tailCallFunction = null
//    fn
//  }

  /**
   * Create a new stack frame for local variables, in cases (such as attribute sets and global variables)
   * where there are no parameters
   * @param numberOfSlots the number of slots needed in the stack frame
   */
  def openStackFrame(numberOfSlots: Int): Unit = {
    stackFrame = if (numberOfSlots == 0) EMPTY_STACKFRAME else new Array[Sequence](numberOfSlots)
  }

//ORBEON XSLT
//  /**
//   * Set the current mode.
//   * @param mode the new current mode
//   */
//  def setCurrentMode(mode: Mode) {
//    this.currentMode = mode
//  }

  /**
   * Get the Controller. May return null when running outside XSLT or XQuery
   * @return the controller for this query or transformation
   */
  def getController(): Controller = controller

  /**
   * Get the Configuration
   * @return the Saxon configuration object
   */
  def getConfiguration(): Configuration = controller.getConfiguration

  /**
   * Set the calling XPathContext
   * @param caller the XPathContext of the calling expression
   */
  def setCaller(caller: XPathContext): Unit = {
    this.caller = caller
  }

  /**
   * Get the calling XPathContext (the next one down the stack). This will be null if unknown, or
   * if the bottom of the stack has been reached.
   * @return the XPathContext of the calling expression
   */
  def getCaller(): XPathContext = caller

  /**
   * Set a new sequence iterator.
   * @param iter the current iterator. The context item, position, and size are determined by reference
   * to the current iterator.
   * @return the new current iterator. This will be a FocusIterator that wraps the supplied iterator,
   * maintaining the values of current() and position() as items are read. Note that this returned
   * iterator MUST be used in place of the original, or the values of current() and position() will
   * be incorrect.
   */
  def setCurrentIterator(iter: SequenceIterator): FocusIterator = {
    currentIterator = if (iter.isInstanceOf[FocusIterator]) iter.asInstanceOf[FocusIterator] else new FocusIterator(iter)
    currentIterator
  }

  /**
   * Set a singleton focus: a context item, with position and size both equal to one
   * @param item the singleton focus
   */
  def setSingletonFocus(item: Item): Unit = {
    val iter = SingletonIterator.makeIterator(item)
    val focus = setCurrentIterator(iter)
    focus.next()
  }

  /**
   * Get the current iterator.
   * This encapsulates the context item, context position, and context size.
   * @return the current iterator, or null if there is no current iterator
   * (which means the context item, position, and size are undefined).
   */
  def getCurrentIterator(): FocusIterator = currentIterator

  /**
   * Get the context position (the position of the context item)
   * @return the context position (starting at one)
   * @throws XPathException if the context position is undefined
   */
  def getContextPosition(): Int = {
    if (currentIterator == null) {
      throw new XPathException("The context position is currently undefined", "FONC0001")
    }
    currentIterator.position()
  }

  /**
   * Get the context item
   * @return the context item, or null if the context item is undefined
   */
  def getContextItem(): Item = {
    if (currentIterator == null) null else currentIterator.current()
  }

  /**
   * Get the context size (the position of the last item in the current node list)
   * @return the context size
   * @throws XPathException if the context position is undefined
   */
  def getLast(): Int = {
    if (currentIterator == null) {
      throw new XPathException("The context size is currently undefined", "FONC0001")
    }
    currentIterator.last()
  }

  /**
   * Get a reference to the local stack frame for variables. Note that it's
   * the caller's job to make a local copy of this.
   * @return array of variables.
   */
  def getStackFrame(): Array[Sequence] = stackFrame

  /**
   * Get the value of a local variable, identified by its slot number
   * @param slotnumber the slot number allocated at compile time to the variable,
   * which identifies its position within the local stack frame
   * @return the value of the variable.
   */
  def evaluateLocalVariable(slotnumber: Int): Sequence = stackFrame(slotnumber)

  /**
   * Set the value of a local variable, identified by its slot number
   * @param slotnumber the slot number allocated at compile time to the variable,
   * which identifies its position within the local stack frame
   * @param value the value of the variable
   */
  def setLocalVariable(slotnumber: Int, value: Sequence): Unit = {
    stackFrame(slotnumber) = value
  }

//ORBEON unused
//  /**
//   * Set a new output destination, supplying the output format details. <BR>
//   * Note that it is the caller's responsibility to close the Writer after use.
//   * @param receiver the new destination
//   * @param isFinal true if this is a "final" output destination, e.g. xsl:result-document; non-final destinations
//   * such as a temporary tree inhibit creating a final destination.
//   * @throws XPathException if any dynamic error occurs; and
//   *     specifically, if an attempt is made to switch to a final output
//   *     destination while writing a temporary tree or sequence @param isFinal true if the destination is a final result tree
//   *     (either the principal output or a secondary result tree); false if not
//   */
//  def changeOutputDestination(receiver: Receiver, isFinal: Boolean) {
//    if (isFinal && isTemporaryDestination) {
//      val err = new XPathException("Cannot switch to a final result destination while writing a temporary tree")
//      err.setErrorCode("XTDE1480")
//      throw err
//    }
//    if (!isFinal) {
//      isTemporaryDestination = true
//    }
//    val pipe = receiver.getPipelineConfiguration
//    val out = new ComplexContentOutputter()
//    out.setPipelineConfiguration(pipe)
//    val ne = new NamespaceReducer()
//    ne.setUnderlyingReceiver(receiver)
//    ne.setPipelineConfiguration(pipe)
//    out.setReceiver(receiver)
//    currentReceiver = out
//  }

  /**
   * Set the SequenceReceiver to which output is to be written, marking it as a temporary (non-final)
   * output destination.
   * @param out The SequenceReceiver to be used
   */
  def setTemporaryReceiver(out: SequenceReceiver): Unit = {
    isTemporaryDestination = true
    currentReceiver = out
  }

  def setTemporaryOutputState(temporary: Boolean): Unit = {
    this.isTemporaryDestination = temporary
  }

  /**
   * Get the Receiver to which output is currently being written.
   * @return the current SequenceReceiver
   */
  def getReceiver(): SequenceReceiver = currentReceiver

//ORBEON XSLT
//  /**
//   * Get the current mode.
//   * @return the current mode
//   */
//  def getCurrentMode(): Mode = {
//    val m = currentMode
//    if (m == null) {
//      val rm = getController.getRuleManager
//      if (rm != null) {
//        rm.getUnnamedMode
//      } else {
//        null
//      }
//    } else {
//      m
//    }
//  }

//ORBEON XSLT
//  /**
//   * Get the current template rule. This is used to support xsl:apply-imports and xsl:next-match
//   * @return the current template rule
//   */
//  def getCurrentTemplateRule(): Rule = currentTemplate

  /**
   * Set the current template. This is used to support xsl:apply-imports. The caller
   * is responsible for remembering the previous current template and resetting it
   * after use.
   *
   * @param rule the current template rule
   */
  def setCurrentTemplateRule(rule: Rule): Unit = {
//ORBEON XSLT
//    this.currentTemplate = rule
  }

  /**
   * Get the implicit timezone
   * @return the implicit timezone. This will be the timezone of the current date and time, and
   * all calls within a single query or transformation will return the same value. The result is
   * expressed as an offset from UTC in minutes.
   */
  def getImplicitTimezone(): Int = getConfiguration.getImplicitTimezone
}

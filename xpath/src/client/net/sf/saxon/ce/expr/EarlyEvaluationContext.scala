package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr.instruct.ParameterSet
import client.net.sf.saxon.ce.expr.sort.GroupIterator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.regex.ARegexIterator
import client.net.sf.saxon.ce.trans.Mode
import client.net.sf.saxon.ce.trans.Rule
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.FocusIterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This class is an implementation of XPathContext used when evaluating constant sub-expressions at
 * compile time.
 */
class EarlyEvaluationContext(var config: Configuration) extends XPathContext(null) {

  /**
   * Set a new output destination, supplying the output format details. <BR>
   * Note that it is the caller's responsibility to close the Writer after use.
   *
   * @param isFinal true if the destination is a final result tree
   *                (either the principal output or a secondary result tree); false if
   *                it is a temporary tree, xsl:attribute, etc.
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any dynamic error occurs; and
   *          specifically, if an attempt is made to switch to a final output
   *          destination while writing a temporary tree or sequence
   */
  def changeOutputDestination(receiver: Receiver, isFinal: Boolean) {
    notAllowed()
  }

  /**
   * Get the value of a local variable, identified by its slot number
   */
  def evaluateLocalVariable(slotnumber: Int): Sequence = {
    notAllowed()
    null
  }

  /**
   * Get the calling XPathContext (the next one down the stack). This will be null if unknown, or
   * if the bottom of the stack has been reached.
   */
  def getCaller(): XPathContext = null

  /**
   * Get the Configuration
   */
  def getConfiguration(): Configuration = config

  /**
   * Get the context item
   *
   * @return the context item, or null if the context item is undefined
   */
  def getContextItem(): Item = null

  /**
   * Get the context position (the position of the context item)
   *
   * @return the context position (starting at one)
   * @throws XPathException
   *          if the context position is undefined
   */
  def getContextPosition(): Int = {
    val err = new XPathException("The context position is undefined")
    err.setErrorCode("FONC0001")
    throw err
  }

  /**
   * Get the Controller. May return null when running outside XSLT or XQuery
   */
  def getController(): Controller = null

  /**
   * Get the current group iterator. This supports the current-group() and
   * current-grouping-key() functions in XSLT 2.0
   *
   * @return the current grouped collection
   */
  def getCurrentGroupIterator(): GroupIterator = {
    notAllowed()
    null
  }

  /**
   * Get the current iterator.
   * This encapsulates the context item, context position, and context size.
   *
   * @return the current iterator, or null if there is no current iterator
   *         (which means the context item, position, and size are undefined).
   */
  def getCurrentIterator(): FocusIterator = null

  /**
   * Get the current mode.
   *
   * @return the current mode
   */
  def getCurrentMode(): Mode = {
    notAllowed()
    null
  }

  /**
   * Get the current regex iterator. This supports the functionality of the regex-group()
   * function in XSLT 2.0.
   *
   * @return the current regular expressions iterator
   */
  def getCurrentRegexIterator(): ARegexIterator = null

  /**
   * Get the current template. This is used to support xsl:apply-imports
   *
   * @return the current template
   */
  def getCurrentTemplateRule(): Rule = null

  /**
   * Get the context size (the position of the last item in the current node list)
   *
   * @return the context size
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if the context position is undefined
   */
  def getLast(): Int = {
    val err = new XPathException("The context item is undefined")
    err.setErrorCode("XPDY0002")
    throw err
  }

  /**
   * Get the local (non-tunnel) parameters that were passed to the current function or template
   *
   * @return a ParameterSet containing the local parameters
   */
  def getLocalParameters(): ParameterSet = {
    notAllowed()
    null
  }

  /**
   * Get the Receiver to which output is currently being written.
   *
   * @return the current Receiver
   */
  def getReceiver(): SequenceReceiver = {
    notAllowed()
    null
  }

  /**
   * Get a reference to the local stack frame for variables. Note that it's
   * the caller's job to make a local copy of this. This is used for creating
   * a Closure containing a retained copy of the variables for delayed evaluation.
   *
   * @return array of variables.
   */
  def getStackFrame(): Array[Sequence] = {
    notAllowed()
    null
  }

  /**
   * Get the tunnel parameters that were passed to the current function or template. This includes all
   * active tunnel parameters whether the current template uses them or not.
   *
   * @return a ParameterSet containing the tunnel parameters
   */
  def getTunnelParameters(): ParameterSet = {
    notAllowed()
    null
  }

  /**
   * Construct a new context without copying (used for the context in a function call)
   */
  def newCleanContext(): XPathContext = {
    notAllowed()
    null
  }

  /**
   * Construct a new context as a copy of another. The new context is effectively added
   * to the top of a stack, and contains a pointer to the previous context
   */
  def newContext(): XPathContext = {
    val controller = new Controller(config)
    controller.newXPathContext()
  }

  /**
   * Construct a new minor context. A minor context can only hold new values of the focus
   * (currentIterator) and current output destination.
   */
  def newMinorContext(): XPathContext = newContext().newMinorContext()

  /**
   * Set the calling XPathContext
   */
  def setCaller(caller: XPathContext) {
  }

  /**
   * Set a new sequence iterator.
   */
  def setCurrentIterator(iter: SequenceIterator): FocusIterator = {
    notAllowed()
    null
  }

  def setSingletonFocus(item: Item) {
    notAllowed()
  }

  /**
   * Set the value of a local variable, identified by its slot number
   */
  def setLocalVariable(slotnumber: Int, value: Sequence) {
    notAllowed()
  }

  /**
   * Set the receiver to which output is to be written, marking it as a temporary (non-final)
   * output destination.
   *
   * @param out The SequenceOutputter to be used
   */
  def setTemporaryReceiver(out: SequenceReceiver) {
    notAllowed()
  }

  /**
   * Get the implicit timezone, as a positive or negative offset from UTC in minutes.
   * The range is -14hours to +14hours. This implementation always throws a
   * NoDynamicContextException.
   * @return the implicit timezone, as an offset from UTC in minutes
   */
  def getImplicitTimezone(): Int = config.getImplicitTimezone

  /**
   * Throw an error for operations that aren't supported when doing early evaluation of constant
   * subexpressions
   */
  private def notAllowed() {
    throw new UnsupportedOperationException("Internal error: early evaluation of subexpression with no context")
  }
}

package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This interface represents an expression that is capable of being processed leaving tail calls for the
 * calling instruction to deal with.
 */
trait TailCallReturner {

  /**
   * ProcessLeavingTail: called to do the real work of this instruction. This method
   * must be implemented in each subclass. The results of the instruction are written
   * to the current Receiver, which can be obtained via the Controller.
   * @param context The dynamic context of the transformation, giving access to the current node,
   * the current variables, etc.
   * @return null if the instruction has completed execution; or a TailCall indicating
   * a function call or template call that is delegated to the caller, to be made after the stack has
   * been unwound so as to save stack space.
   */
  def processLeavingTail(context: XPathContext): TailCall
}

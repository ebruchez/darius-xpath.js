// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.event.SequenceReceiver
import org.orbeon.darius.xpath.om.{Item, Sequence, SequenceIterator}
import org.orbeon.darius.xpath.orbeon.{Configuration, Controller}
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.FocusIterator

/**
 * This class is an implementation of XPathContext used when evaluating constant sub-expressions at
 * compile time.
 */
class EarlyEvaluationContext(var config: Configuration) extends XPathContext(null) {

  override def evaluateLocalVariable(slotnumber: Int)             = notAllowed()
  override def getCaller: XPathContext                            = null
  override def getConfiguration                                   = config
  override def getContextItem                                     = null

  override def getContextPosition =
    throw new XPathException("The context position is undefined", "FONC0001")

  override def getController                                      = null
  override def getCurrentIterator                                 = null

  override def getLast =
    throw new XPathException("The context item is undefined", "XPDY0002")

  override def getReceiver                                        = notAllowed()
  override def getStackFrame                                      = notAllowed()

  override def newCleanContext()                                  = notAllowed()

  override def newContext(): XPathContext                         = new Controller(config).newXPathContext()
  override def newMinorContext(): XPathContext                    = newContext().newMinorContext()
  override def setCaller(caller: XPathContext)                    = ()

  override def setCurrentIterator(iter: SequenceIterator)         = notAllowed()
  override def setSingletonFocus(item: Item)                      = notAllowed()
  override def setLocalVariable(slotnumber: Int, value: Sequence) = notAllowed()
  override def setTemporaryReceiver(out: SequenceReceiver)        = notAllowed()

  override def getImplicitTimezone                                = config.getImplicitTimezone

  private def notAllowed() =
    throw new UnsupportedOperationException("Internal error: early evaluation of subexpression with no context")

//ORBEON unused
//  def changeOutputDestination(receiver: Receiver, isFinal: Boolean) = notAllowed()
//ORBEON XSLT
//  override def getCurrentGroupIterator() = notAllowed()
//  def getCurrentMode() = notAllowed()
//  override def getCurrentRegexIterator() = null
//  def getCurrentTemplateRule() = null
//  override def getLocalParameters() = notAllowed()
//  override def getTunnelParameters() = notAllowed()

}

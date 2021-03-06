// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.event

import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.Controller
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This filter is inserted into the serializer pipeline when serializing an implicit XSLT result tree, that
 * is, one that is created without use of xsl:result-document. Its main purpose is to check, if and only if
 * the result destination is actually written to, that it does not conflict with an explicit result destination
 * with the same URI. It also ensures that the output destination is opened before it is first written to.
 */
class ImplicitResultChecker(next: Receiver, var controller: Controller) extends ProxyReceiver {

  private var clean: Boolean = true

  private var open: Boolean = false

  setUnderlyingReceiver(next)

  def open(): Unit = {
    super.open()
    open = true
  }

  def startDocument(): Unit = {
    if (!open) {
      open()
    }
    nextReceiver.startDocument()
  }

  def startElement(qName: StructuredQName, properties: Int): Unit = {
    if (clean) {
      firstContent()
    }
    nextReceiver.startElement(qName, properties)
  }

  def characters(chars: CharSequence): Unit = {
    if (clean) {
      firstContent()
    }
    nextReceiver.characters(chars)
  }

  def processingInstruction(target: String, data: CharSequence): Unit = {
    if (clean) {
      firstContent()
    }
    nextReceiver.processingInstruction(target, data)
  }

  def comment(chars: CharSequence): Unit = {
    if (clean) {
      firstContent()
    }
    nextReceiver.comment(chars)
  }

  /**
   * This method does the real work. It is called when the first output is written to the implicit output
   * destination, and checks that no explicit result document has been written to the same URI
   * as the implicit result document
   * @throws XPathException
   */
  private def firstContent(): Unit = {
    controller.checkImplicitResultTree()
    if (!open) {
      open()
      startDocument()
    }
    clean = false
  }

  def close(): Unit = {
    if (!clean || !controller.hasThereBeenAnExplicitResultDocument()) {
      if (!open) {
        open()
      }
      nextReceiver.close()
    }
  }
}

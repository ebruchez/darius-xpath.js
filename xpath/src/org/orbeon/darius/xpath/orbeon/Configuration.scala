// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.orbeon

import java.net.URI
import java.util.Date

import org.orbeon.darius.xpath.expr.sort.{CaseInsensitiveCollator, CodepointCollator}
import org.orbeon.darius.xpath.lib.{ErrorListener, NamespaceConstant, StandardErrorListener, StringCollator}
import org.orbeon.darius.xpath.om.{DocumentInfo, DocumentPool}
import org.orbeon.darius.xpath.value.DateTimeValue

import scala.beans.BeanProperty

object Configuration {

  def getEditionCode: String = "CE"

  def getLocation: URI = {
    ???
//    var location: URI = null
//    try {
//      location = new URI(Window.Location.getHref)
//    } catch {
//      case err: Exception ⇒
//    }
//    location
  }
}

class Configuration {

  @BeanProperty
  var errorListener: ErrorListener = new StandardErrorListener()

  @BeanProperty
  var globalDocumentPool: DocumentPool = new DocumentPool()

  @BeanProperty
  var implicitTimezone: Int = DateTimeValue.fromJavaDate(new Date()).getTimezoneInMinutes

  private val sourceDocumentPool: DocumentPool = new DocumentPool()

  private val logger: Logger = Logger.getLogger("Configuration")

  private var nextDocumentNumber: Int = 0

  def allocateDocumentNumber(): Int = {
    synchronized {
      val result = nextDocumentNumber
      nextDocumentNumber += 1
      result
    }
  }

  def getNamedCollation(name: String): StringCollator = {
    if (name == NamespaceConstant.CODEPOINT_COLLATION_URI) {
      CodepointCollator.getInstance
    } else if (name == NamespaceConstant.CASE_INSENSITIVE_COLLATION_URI) {
      CaseInsensitiveCollator.getInstance
    } else {
      null
    }
  }

  def getDocumentPool: DocumentPool = sourceDocumentPool

  def issueWarning(message: String): Unit =
    logger.warning(message)

  def buildDocument(url: String): DocumentInfo = {
    ??? //ORBEON
  }
}

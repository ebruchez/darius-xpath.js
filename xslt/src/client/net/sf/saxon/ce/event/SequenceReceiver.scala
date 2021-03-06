// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.event

import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * SequenceReceiver: this extension of the Receiver interface is used when processing
 * a sequence constructor. It differs from the Receiver in allowing items (atomic values or
 * nodes) to be added to the sequence, not just tree-building events.
 */
abstract class SequenceReceiver extends Receiver {

  protected var previousAtomic: Boolean = false

  protected var pipelineConfiguration: PipelineConfiguration = _

  protected var systemId: String = null

  def getPipelineConfiguration: PipelineConfiguration = pipelineConfiguration

  def setPipelineConfiguration(pipelineConfiguration: PipelineConfiguration): Unit = {
    this.pipelineConfiguration = pipelineConfiguration
  }

  /**
   * Get the Saxon Configuration
   * @return the Configuration
   */
  def getConfiguration(): Configuration = pipelineConfiguration.getConfiguration

  /**
   * Set the system ID
   * @param systemId the URI used to identify the tree being passed across this interface
   */
  def setSystemId(systemId: String): Unit = {
    this.systemId = systemId
  }

  /**
   * Get the system ID
   * @return the system ID that was supplied using the setSystemId() method
   */
  def getSystemId(): String = systemId

  /**
   * Start the output process
   */
  def open(): Unit = {
    previousAtomic = false
  }

  /**
   * Append an arbitrary item (node or atomic value) to the output
   * @param item           the item to be appended
   * @param copyNamespaces if the item is an element node, this indicates whether its namespaces
   *                       need to be copied. Values are [[org.orbeon.darius.xpath.om.NodeInfo#ALL_NAMESPACES]],
   *                       [[org.orbeon.darius.xpath.om.NodeInfo#LOCAL_NAMESPACES]], [[org.orbeon.darius.xpath.om.NodeInfo#NO_NAMESPACES]]
   */
  def append(item: Item, copyNamespaces: Int): Unit

  /**
   * Append an item (node or atomic value) to the output
   * @param item the item to be appended
   */
  def append(item: Item): Unit = {
    append(item, NodeInfo.ALL_NAMESPACES)
  }
}

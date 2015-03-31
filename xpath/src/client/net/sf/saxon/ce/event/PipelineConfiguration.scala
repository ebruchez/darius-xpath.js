// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.lib.ErrorListener
import client.net.sf.saxon.ce.orbeon.{Configuration, Controller}

import scala.beans.BeanProperty

/**
 * A PipelineConfiguration sets options that apply to all the operations in a pipeline.
 * Unlike the global Configuration, these options are always local to a process.
 */
class PipelineConfiguration {

  private var config: Configuration = _

  @BeanProperty
  var errorListener: ErrorListener = _

  @BeanProperty
  var controller: Controller = _

  /**
   * Create a PipelineConfiguration as a copy of an existing
   * PipelineConfiguration
   * @param p the existing PipelineConfiguration
   */
  def this(p: PipelineConfiguration) {
    this()
    config = p.config
    controller = p.controller
  }

  /**
   * Get the Saxon Configuration object
   * @return the Saxon Configuration
   */
  def getConfiguration(): Configuration = config

  /**
   * Set the Saxon Configuration object
   * @param config the Saxon Configuration
   */
  def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }
}

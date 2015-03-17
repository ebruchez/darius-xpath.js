package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.Configuration
import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.lib.ErrorListener
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

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
  def setConfiguration(config: Configuration) {
    this.config = config
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The abstract Builder class is responsible for taking a stream of SAX events
 * and constructing a Document tree. There is one concrete subclass for each
 * tree implementation.
 * @author Michael H. Kay
 */
abstract class Builder extends Receiver {

  protected var pipe: PipelineConfiguration = _

  protected var config: Configuration = _

  protected var systemId: String = _

  protected var baseURI: String = _

  protected var currentRoot: NodeInfo = _

  protected var started: Boolean = false

  protected var open: Boolean = false

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
    config = pipe.getConfiguration
  }

  def getPipelineConfiguration: PipelineConfiguration = pipe

  /**
   * Get the Configuration
   * @return the Saxon configuration
   */
  def getConfiguration(): Configuration = config

  /**
   * The SystemId is equivalent to the document-uri property defined in the XDM data model.
   * It should be set only in the case of a document that is potentially retrievable via this URI.
   * This means it should not be set in the case of a temporary tree constructed in the course of
   * executing a query or transformation.
   * @param systemId the SystemId, that is, the document-uri.
   */
  def setSystemId(systemId: String): Unit = {
    this.systemId = systemId
  }

  /**
   * The SystemId is equivalent to the document-uri property defined in the XDM data model.
   * It should be set only in the case of a document that is potentially retrievable via this URI.
   * This means the value will be null in the case of a temporary tree constructed in the course of
   * executing a query or transformation.
   * @return the SystemId, that is, the document-uri.
   */
  def getSystemId(): String = systemId

  /**
   * Set the base URI of the document node of the tree being constructed by this builder
   * @param baseURI the base URI
   */
  def setBaseURI(baseURI: String): Unit = {
    this.baseURI = baseURI
  }

  /**
   * Get the base URI of the document node of the tree being constructed by this builder
   * @return the base URI
   */
  def getBaseURI(): String = baseURI

  def open(): Unit = {
    open = true
  }

  def close(): Unit = {
    open = false
  }

  /**
   * Get the current root node. This will normally be a document node, but if the root of the tree
   * is an element node, it can be an element.
   * @return the root of the tree that is currently being built, or that has been most recently built
   * using this builder
   */
  def getCurrentRoot(): NodeInfo = currentRoot

  /**
   * Reset the builder to its initial state. The most important effect of calling this
   * method (implemented in subclasses) is to release any links to the constructed document
   * tree, allowing the memory occupied by the tree to released by the garbage collector even
   * if the Builder is still in memory. This can happen because the Builder is referenced from a
   * parser in the Configuration's parser pool.
   */
  def reset(): Unit = {
    pipe = null
    config = null
    systemId = null
    baseURI = null
    currentRoot = null
    started = false
    open = false
  }
}

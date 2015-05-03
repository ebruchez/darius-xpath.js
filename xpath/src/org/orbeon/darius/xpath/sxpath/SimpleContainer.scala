// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.sxpath

import org.orbeon.darius.xpath.expr.Container
import org.orbeon.darius.xpath.orbeon.Executable
import org.orbeon.darius.xpath.tree.util.SourceLocator

import scala.beans.BeanProperty

/**
 * A simple container for standalone XPath expressions
 */
class SimpleContainer(exec: Executable) extends Container with SourceLocator {

  @BeanProperty
  var executable: Executable = exec

  /**
   * Get the LocationProvider allowing location identifiers to be resolved.
   *
   * @return the location provider
   */
  def getSourceLocator: SourceLocator = this

  /**
   * Get the granularity of the container. During successive phases of compilation, growing
   * expression trees are rooted in containers of increasing granularity. The granularity
   * of the container is used to avoid "repotting" a tree more frequently than is required,
   * as this requires a complete traversal of the tree which can take a measurable time.
   *
   * @return 0 for a temporary container created during parsing; 1 for a container
   *         that operates at the level of an XPath expression; 2 for a container at the level
   *         of a global function or template
   */
  def getContainerGranularity: Int = 1

  /**
   * Return the system identifier for the current document event.
   * <p/>
   * <p>The return value is the system identifier of the document
   * entity or of the external parsed entity in which the markup that
   * triggered the event appears.</p>
   * <p/>
   * <p>If the system identifier is a URL, the parser must resolve it
   * fully before passing it to the application.</p>
   *
   * @return A string containing the system identifier, or null
   *         if none is available.
   */
  def getSystemId: String = null

  def getLocation: String = null
}

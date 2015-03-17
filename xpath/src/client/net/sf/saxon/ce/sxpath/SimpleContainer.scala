package client.net.sf.saxon.ce.sxpath

import client.net.sf.saxon.ce.expr.Container
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.tree.util.SourceLocator
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

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
  def getSourceLocator(): SourceLocator = this

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
  def getContainerGranularity(): Int = 1

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
  def getSystemId(): String = null

  def getLocation(): String = null
}

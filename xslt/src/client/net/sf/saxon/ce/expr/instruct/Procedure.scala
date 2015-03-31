// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.Container
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionTool
import client.net.sf.saxon.ce.trace.InstructionInfo
import client.net.sf.saxon.ce.tree.util.SourceLocator
import java.util.Collections
import java.util.Iterator
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This object represents the compiled form of a user-written function, template, attribute-set, etc
 * (the source can be either an XSLT stylesheet function or an XQuery function).
 *
 * <p>It is assumed that type-checking, of both the arguments and the results,
 * has been handled at compile time. That is, the expression supplied as the body
 * of the function must be wrapped in code to check or convert the result to the
 * required type, and calls on the function must be wrapped at compile time to check or
 * convert the supplied arguments.
 */
abstract class Procedure extends Container with InstructionInfo {

  @BeanProperty
  var sourceLocator: SourceLocator = _

  protected var body: Expression = _

  @BeanProperty
  val executable: Executable = _

  @BeanProperty
  var numberOfSlots: Int = _

  /**
   * Get the granularity of the container.
   * @return 0 for a temporary container created during parsing; 1 for a container
   *         that operates at the level of an XPath expression; 2 for a container at the level
   *         of a global function or template
   */
  def getContainerGranularity(): Int = 2

  def setBody(body: Expression): Unit = {
    this.body = body
    body.setContainer(this)
  }

  def getBody(): Expression = body

  def allocateSlots(reserved: Int): Unit = {
    numberOfSlots = ExpressionTool.allocateSlots(body, reserved)
  }

  def getProperty(name: String): AnyRef = null

  def getLineNumber(): Int = -1

  def getSystemId(): String = ""

  /**
   * Get an iterator over all the properties available. The values returned by the iterator
   * will be of type String, and each string can be supplied as input to the getProperty()
   * method to retrieve the value of the property. The iterator may return properties whose
   * value is null.
   */
  def getProperties(): Iterator = Collections.EMPTY_LIST.iterator()
}

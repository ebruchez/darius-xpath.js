// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.trace.Location
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceType
import java.util.Iterator
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The runtime object corresponding to an xsl:template element in the stylesheet.
 *
 * Note that the Template object no longer has precedence information associated with it; this is now
 * only in the Rule object that references this Template. This allows two rules to share the same template,
 * with different precedences. This occurs when a stylesheet module is imported more than once, from different
 * places, with different import precedences.
 */
class Template extends Procedure {

  @BeanProperty
  var matchPattern: Pattern = _

  @BeanProperty
  var templateName: StructuredQName = _

  var hasRequiredParams: Boolean = _

  private var bodyIsTailCallReturner: Boolean = _

  private var requiredType: SequenceType = _

  /**
   * Set the expression that forms the body of the template
   * @param body the body of the template
   */
  def setBody(body: Expression): Unit = {
    super.setBody(body)
    bodyIsTailCallReturner = body.isInstanceOf[TailCallReturner]
  }

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   *
   */
  def getObjectName(): StructuredQName = templateName

  /**
   * Set whether this template has one or more required parameters
   * @param has true if the template has at least one required parameter
   */
  def setHasRequiredParams(has: Boolean): Unit = {
    hasRequiredParams = has
  }

  /**
   * Set the required type to be returned by this template
   * @param type the required type as defined in the "as" attribute on the xsl:template element
   */
  def setRequiredType(`type`: SequenceType): Unit = {
    requiredType = `type`
  }

  /**
   * Get the required type to be returned by this template
   * @return the required type as defined in the "as" attribute on the xsl:template element
   */
  def getRequiredType(): SequenceType = {
    if (requiredType == null) {
      SequenceType.ANY_SEQUENCE
    } else {
      requiredType
    }
  }

  /**
   * Get the local parameter with a given parameter id
   * @param id the parameter id
   * @return the local parameter with this id if found, otherwise null
   */
  def getLocalParam(id: Int): LocalParam = {
    val iter = body.iterateSubExpressions()
    while (iter.hasNext) {
      val child = iter.next()
      if (child.isInstanceOf[LocalParam] && 
        child.asInstanceOf[LocalParam].getParameterId == id) {
        return child.asInstanceOf[LocalParam]
      }
    }
    null
  }

  /**
   * Process the template, without returning any tail calls. This path is used by
   * xsl:apply-imports and xsl:next-match
   * @param context The dynamic context, giving access to the current node,
   */
  def apply(context: XPathContext): Unit = {
    var tc = applyLeavingTail(context)
    while (tc != null) {
      tc = tc.processLeavingTail()
    }
  }

  /**
   * Process this template, with the possibility of returning a tail call package if the template
   * contains any tail calls that are to be performed by the caller.
   * @param context the XPath dynamic context
   * @return null if the template exited normally; but if it was a tail call, details of the call
   * that hasn't been made yet and needs to be made by the caller
   */
  def applyLeavingTail(context: XPathContext): TailCall = {
    if (bodyIsTailCallReturner) {
      body.asInstanceOf[TailCallReturner].processLeavingTail(context)
    } else {
      body.process(context)
      null
    }
  }

  /**
   * Expand the template. Called when the template is invoked using xsl:call-template.
   * Invoking a template by this method does not change the current template.
   * @param context the XPath dynamic context
   * @return null if the template exited normally; but if it was a tail call, details of the call
   * that hasn't been made yet and needs to be made by the caller
   */
  def expand(context: XPathContext): TailCall = {
    if (bodyIsTailCallReturner) {
      return body.asInstanceOf[TailCallReturner].processLeavingTail(context)
    } else if (body != null) {
      body.process(context)
    }
    null
  }

  def getConstructType(): StructuredQName = Location.TEMPLATE
}

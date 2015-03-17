// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import AttributeSet._
//remove if not needed
import scala.collection.JavaConversions._

object AttributeSet {

  /**
   * Expand an array of attribute sets
   * @param asets the attribute sets to be expanded
   * @param context the run-time context to use
   * @throws XPathException if a dynamic error occurs
   */
  protected def expand(asets: Array[AttributeSet], context: XPathContext) {
    for (aset <- asets) {
      aset.expand(context)
    }
  }
}

class AttributeSet extends Procedure {

  var attributeSetName: StructuredQName = _

  private var useAttributeSets: Array[AttributeSet] = _

  /**
   * Set the name of the attribute-set
   * @param attributeSetName the name of the attribute-set
   */
  def setName(attributeSetName: StructuredQName) {
    this.attributeSetName = attributeSetName
  }

  /**
   * Set the attribute sets used by this attribute set
   * @param useAttributeSets the set of attribute sets used by this attribute set
   */
  def setUseAttributeSets(useAttributeSets: Array[AttributeSet]) {
    this.useAttributeSets = useAttributeSets
  }

  /**
   * Determine whether the attribute set has any dependencies on the focus
   * @return the dependencies
   */
  def getFocusDependencies(): Int = {
    var d = 0
    if (body != null) {
      d |= body.getDependencies & StaticProperty.DEPENDS_ON_FOCUS
    }
    if (useAttributeSets != null) {
      for (useAttributeSet <- useAttributeSets) {
        d |= useAttributeSet.getFocusDependencies
      }
    }
    d
  }

  /**
   * Evaluate an attribute set
   * @param context the dynamic context
   * @throws XPathException if any failure occurs
   */
  def expand(context: XPathContext) {
    if (useAttributeSets != null) {
      AttributeSet.expand(useAttributeSets, context)
    }
    if (getNumberOfSlots != 0) {
      val c2 = context.newContext()
      c2.openStackFrame(getNumberOfSlots)
      getBody.process(c2)
    } else {
      getBody.process(context)
    }
  }

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   *
   */
  def getObjectName(): StructuredQName = attributeSetName

  def getConstructType(): StructuredQName = {
    new StructuredQName("xsl", NamespaceConstant.XSLT, "attribute-set")
  }
}

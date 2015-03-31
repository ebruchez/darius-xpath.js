// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans.update

import client.net.sf.saxon.ce.dom.HTMLWriter
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.trans.XPathException
import com.google.gwt.dom.client.Element
import com.google.gwt.dom.client.Node
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A pending update action representing the effect of a delete expression
 */
class SetAttributeAction(element: Element, 
    var uri: String, 
    localNname: String, 
    var value: String) extends PendingUpdateAction {

  @BeanProperty
  var targetNode: Element = element

  private var localName: String = localNname

  /**
   * Apply the pending update action to the affected node
   *
   * @param context the XPath evaluation context
   */
  def apply(context: XPathContext): Unit = {
    if (value == null) {
      targetNode.removeAttribute(localName)
    } else if (NamespaceConstant.HTML_PROP == uri) {
      targetNode.setPropertyString(localName, value)
    } else if (NamespaceConstant.HTML_STYLE_PROP == uri) {
      var name: String = null
      name = if (localName.length > 1 && localName.charAt(0) == '_' && localName.charAt(1) == '-') localName.substring(1) else localName
      name = HTMLWriter.getCamelCaseName(name)
      targetNode.getStyle.setProperty(name, value)
    } else {
      if (uri.length == 0) {
        targetNode.setAttribute(localName, value)
        HTMLWriter.setAttributeProps(targetNode, localName, value)
      } else {
        HTMLWriter.setAttribute(targetNode.getOwnerDocument, targetNode, localName, uri, value, HTMLWriter.WriteMode.HTML)
      }
      if (localName == "style") {
        HTMLWriter.setStyleProperties(targetNode, value)
      }
    }
  }

  private /* native */ def getNodeNamespace(node: Node): String
}

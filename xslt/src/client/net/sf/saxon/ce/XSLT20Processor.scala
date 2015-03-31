// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce

import org.timepedia.exporter.client.Export
import org.timepedia.exporter.client.ExportPackage
import org.timepedia.exporter.client.Exportable
import client.net.sf.saxon.ce.Controller.APIcommand
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.JsArrayIterator
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.dom.client.Document
import com.google.gwt.dom.client.Node
//remove if not needed
import scala.collection.JavaConversions._

@Export(value = "XSLT20Processor")
@ExportPackage("Saxonce")
class XSLT20Processor extends Exportable {

  var processor: Xslt20ProcessorImpl = new Xslt20ProcessorImpl()

  var controller: Controller = processor.getController

  SaxonceApi.setProcessorWasJsInitiated()

  def this(stylesheet: JavaScriptObject) {
    this()
    if (stylesheet != null) {
      processor.importStylesheet(stylesheet)
    }
  }

  private var jsThis: JavaScriptObject = null

  /**
   * Keep instance of the JavaScript wrapped version - to
   * be used when making a callback
   */
  def setThis(jsObj: JavaScriptObject): Unit = {
    jsThis = jsObj
  }

  /**
   * Erases any parameter values set using setParameter method
   */
  def clearParameters(): Unit = {
    controller.clearParameters()
  }

  /**
   * Returns the value of the parameter as a String.
   * @param namespaceURI the parameter namespace
   * @param localName the local name of the parameter
   * @return the string value of the parameter
   */
  def getParameter(namespaceURI: String, localName: String): AnyRef = {
    val ns = if ((namespaceURI == null)) "" else namespaceURI
    try {
      val qn = new StructuredQName("", ns, localName)
      val vr = controller.getParameter(qn)
      return IXSLFunction.convertToJavaScript(vr)
    } catch {
      case e: Exception => Xslt20ProcessorImpl.handleException(e, "getParameter")
    }
    null
  }

  def importStylesheet(stylesheet: JavaScriptObject): Unit = {
    processor.importStylesheet(stylesheet)
  }

  def setSuccess(success: JavaScriptObject): Unit = {
    processor.setSuccess(success, this)
  }

  def getSuccess(): JavaScriptObject = processor.getSuccess

  def invokeSuccess(callback: JavaScriptObject): Unit = {
    executeSuccessCallback(callback, jsThis)
  }

  /* native */ def executeSuccessCallback(callback: JavaScriptObject, proc: JavaScriptObject): Unit

  /**
   * Deletes a parameter value set using the setParameter method
   * @param namespaceURI the parameter namespace
   * @param localName the local name of the parameter
   */
  def removeParameter(namespaceURI: String, localName: String): Unit = {
    val ns = if ((namespaceURI == null)) "" else namespaceURI
    try {
      val qn = new StructuredQName("", ns, localName)
      controller.removeParameter(qn)
    } catch {
      case e: Exception => Xslt20ProcessorImpl.handleException(e, "getParameter")
    }
  }

  /**
   * Restore the XSLTProcessor20 instance to its default state
   */
  def reset(): Unit = {
    controller.reset()
    processor.deregisterEventHandlers()
  }

  def setParameter(namespaceURI: String, localName: String, value: AnyRef): Unit = {
    val ns = if ((namespaceURI == null)) "" else namespaceURI
    try {
      val qn = new StructuredQName("", ns, localName)
      controller.setParameter(qn, getValue(value))
    } catch {
      case e: Exception => Xslt20ProcessorImpl.handleException(e, "setParameter")
    }
  }

  private def getValue(jso: AnyRef): Sequence = {
    val iterator = IXSLFunction.convertFromJavaScript(jso, processor.config)
    if (iterator.isInstanceOf[JsArrayIterator]) {
      iterator.asInstanceOf[Sequence]
    } else {
      iterator.next()
    }
  }

  def updateHTMLDocument(sourceObject: JavaScriptObject, targetDocument: Document): Unit = {
    processor.updateHTMLDocument(sourceObject, targetDocument, APIcommand.UPDATE_HTML)
  }

  def transformToHTMLFragment(sourceObject: JavaScriptObject, targetDocument: Document): Unit = {
    processor.updateHTMLDocument(sourceObject, targetDocument, APIcommand.TRANSFORM_TO_HTML_FRAGMENT)
  }

  def transformToDocument(sourceNode: JavaScriptObject): Document = {
    processor.transformToDocument(sourceNode).asInstanceOf[Document]
  }

  def transformToFragment(sourceNode: JavaScriptObject, ownerDocument: Document): Node = {
    processor.transformToFragment(sourceNode, ownerDocument)
  }

  def getInitialMode(): String = controller.getInitialMode

  def getInitialTemplate(): String = controller.getInitialTemplate

  def setInitialMode(mode: String): Unit = {
    try {
      controller.setInitialMode(mode)
    } catch {
      case e: Exception => Xslt20ProcessorImpl.handleException(e, "setInitialMode")
    }
  }

  def setInitialTemplate(template: String): Unit = {
    try {
      controller.setInitialTemplate(template)
    } catch {
      case e: Exception => Xslt20ProcessorImpl.handleException(e, "setInitialTemplate")
    }
  }

  def setBaseOutputURI(URI: String): Unit = {
    controller.setBaseOutputURI(URI)
  }

  /**
   * Return result-documents as a JS map of URI/dom name/value pairs
   * Note that the base-output-uri setting is use to resolve relative uris
   * @return
   */
  def getResultDocuments(): JavaScriptObject = controller.getResultDocURIArray

  def getResultDocument(URI: String): JavaScriptObject = controller.getResultDocument(URI)

  def setCollation(): Unit = {
  }
}

package client.net.sf.saxon.ce.dom

import client.net.sf.saxon.ce.trans.XPathException
import com.google.gwt.core.client.JavaScriptException
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.dom.client.Document
import com.google.gwt.dom.client.Node
//remove if not needed
import scala.collection.JavaConversions._

object XMLDOM {

  /**
   * Create a new XML DOM Document object
   * @return a new empty XML DOM Document
   */
  /* native */ def createDocument(baseURI: String): Document

  def makeHTTPRequest(url: String): String = makeNativeHTTPRequest(url)

  def parseXML(text: String): JavaScriptObject = parseNativeXML(text)

  def serializeXML(node: Node): String = serializeNativeXML(node)

  /* native */ def makeNativeHTTPRequest(url: String): String

  /* native */ def parseNativeXML(text: String): JavaScriptObject

  /* native */ def serializeNativeXML(node: Node): String
}

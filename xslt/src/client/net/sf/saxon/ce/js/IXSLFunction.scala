// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.js

import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper.DocType
import client.net.sf.saxon.ce.dom.HTMLNodeWrapper
import client.net.sf.saxon.ce.dom.HTMLWriter
import client.net.sf.saxon.ce.dom.XMLDOM
import client.net.sf.saxon.ce.event.NamespaceReducer
import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.JsArrayIterator
import client.net.sf.saxon.ce.tree.iter.SingletonIterator
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value._
import client.net.sf.saxon.ce.value.StringValue
import com.google.gwt.core.client.JavaScriptException
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.core.client.JsArray
import com.google.gwt.core.client.ScriptInjector
import com.google.gwt.dom.client.Document
import com.google.gwt.dom.client.Node
import com.google.gwt.user.client.Event
import java.util.logging.Logger
import IXSLFunction._
//remove if not needed
import scala.collection.JavaConversions._

object IXSLFunction {

  private var injectCount: Int = 0

  private /* native */ def jsWindow(): JavaScriptObject

  /**
   * Call a JavaScript function (a method on a JavaScript object)
   *
   * @param target    the JavaScript object owning the method
   * @param member    the name of the function to be called as a string
   * @param arguments the arguments to the method call
   * @return the result of evaluating the expression. To ensure that this is always
   *         an Object, it is returned as a type/value pair
   */
  private /* native */ def jsCall(target: JavaScriptObject, member: String, arguments: JavaScriptObject): JavaScriptObject

  private /* native */ def jsProperty(target: JavaScriptObject, member: String): AnyRef

  /**
   * Set a JavaScript property value (a property of a JavaScript object)
   *
   * @param target the JavaScript object owning the method
   * @param member the name of the property to be fetched as a string - may be
   *               a chained set of properties e.g. loction.href
   */
  /* native */ def setProperty(target: JavaScriptObject, member: String, value: AnyRef): Unit

  /**
   * Get a JavaScript property (a property of a JavaScript object)
   *
   * @param target the JavaScript object owning the method
   * @param member the name of the property to be fetched as a string - may be
   *               a chained set of properties e.g. loction.href
   * @return the result of evaluating the expression. To ensure that this is always
   *         an Object, it is returned as a type/value pair
   */
  private /* native */ def jsSplitPropertyTypeAndValue(target: JavaScriptObject, member: String): JavaScriptObject

  private /* native */ def jsNumberProperty(target: JavaScriptObject, member: String): Double

  private /* native */ def jsBooleanProperty(target: JavaScriptObject, member: String): Boolean

  /* native */ def jsArray(length: Int): JavaScriptObject

  /* native */ def jsSetArrayItem(array: JavaScriptObject, index: Int, value: AnyRef): Unit

  private /* native */ def jsGetArrayItem(array: JavaScriptObject, index: Int): AnyRef

  private /* native */ def jsGetArrayLength(array: JavaScriptObject): Int

  /**
   * Special issues with determining if an array - because other object
   * types such as Window are represented as arrays of length 1
   */
  private /* native */ def isJsArray(obj: JavaScriptObject): Boolean

  def convertFromJavaScript(jsValue: AnyRef, config: Configuration): UnfailingIterator = {
    if (jsValue == null) {
      return EmptyIterator.getInstance
    } else if (jsValue.isInstanceOf[String]) {
      return SingletonIterator.makeIterator(new StringValue(jsValue.asInstanceOf[String]))
    } else if (jsValue.isInstanceOf[java.lang.Double]) {
      return SingletonIterator.makeIterator(new DoubleValue(jsValue.asInstanceOf[java.lang.Double]))
    } else if (jsValue.isInstanceOf[java.lang.Boolean]) {
      return SingletonIterator.makeIterator(BooleanValue.get(jsValue.asInstanceOf[java.lang.Boolean]))
    } else if (!(jsValue.isInstanceOf[JavaScriptObject])) {
      return EmptyIterator.getInstance
    }
    val jsObj = jsValue.asInstanceOf[JavaScriptObject]
    val nodeType = getNodeType(jsObj)
    if (nodeType == -1) {
      if (isJsArray(jsObj) && jsGetArrayLength(jsObj) > 1) {
        return new JsArrayIterator(jsObj.asInstanceOf[JsArray], config)
      } else {
        return SingletonIterator.makeIterator(new JSObjectValue(jsObj))
      }
    }
    val page = jsValue.asInstanceOf[Node].getOwnerDocument
    if (page == null) {
      val doc = jsValue.asInstanceOf[Document]
      val jsDocType = if ((doc == Document.get)) DocType.UNKNOWN else DocType.NONHTML
      val docWrapper = new HTMLDocumentWrapper(doc, doc.getURL, config, jsDocType)
      SingletonIterator.makeIterator(docWrapper)
    } else {
      val jsDocType = if ((page == Document.get)) DocType.UNKNOWN else DocType.NONHTML
      val htmlDoc = new HTMLDocumentWrapper(page, page.getURL, config, jsDocType)
      val htmlNode = htmlDoc.wrap(jsValue.asInstanceOf[Node])
      SingletonIterator.makeIterator(htmlNode)
    }
  }

  private /* native */ def getNodeType(obj: JavaScriptObject): Short

  def convertToJavaScript(`val`: Sequence): AnyRef = {
    if (`val` == null || `val`.isInstanceOf[EmptySequence]) {
      return null
    }
    if (`val`.isInstanceOf[Item]) {
      if (`val`.isInstanceOf[StringValue]) {
        `val`.asInstanceOf[StringValue].getStringValue
      } else if (`val`.isInstanceOf[BooleanValue]) {
        `val`.asInstanceOf[BooleanValue].getBooleanValue
      } else if (`val`.isInstanceOf[NumericValue]) {
        `val`.asInstanceOf[NumericValue].getDoubleValue
      } else if (`val`.isInstanceOf[HTMLNodeWrapper]) {
        `val`.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode
      } else if (`val`.isInstanceOf[JSObjectValue]) {
        `val`.asInstanceOf[JSObjectValue].getJavaScriptObject
      } else {
        throw new XPathException("Cannot convert " + `val`.getClass + " to Javascript object")
      }
    } else {
      val seqLength = `val`.getLength
      if (seqLength == 0) {
        null
      } else if (seqLength == 1) {
        convertToJavaScript(`val`.itemAt(0))
      } else {
        convertSequenceToArray(`val`, seqLength)
      }
    }
  }

  private def convertSequenceToArray(`val`: Sequence, seqLength: Int): JavaScriptObject = {
    val jsItems = jsArray(seqLength)
    val iterator = `val`.iterate()
    var i = 0
    while (true) {
      val item = iterator.next()
      if (item == null) {
        //break
      }
      val jsObject = convertToJavaScript(item)
      jsSetArrayItem(jsItems, i, jsObject)
      i += 1
    }
    jsItems
  }
}

/**
 * This class implements Saxon-CE extension functions designed to allow interoperability between XSLT
 * and JavaScript.
 */
class IXSLFunction(var localName: String, arguments: Array[Expression]) extends FunctionCall {

  private var logger: Logger = Logger.getLogger("IXSLFunction")

  setArguments(arguments)

  override def getFunctionName(): StructuredQName = {
    new StructuredQName(NamespaceConstant.IXSL, "", localName)
  }

  protected override def checkArguments(visitor: ExpressionVisitor) {
  }

  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  override def getItemType(): ItemType = AnyItemType.getInstance

  protected override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  protected override def computeSpecialProperties(): Int = StaticProperty.HAS_SIDE_EFFECTS

  private def getValueFromTypeValuePair(pair: JavaScriptObject): AnyRef = {
    val `type` = jsProperty(pair, "type").asInstanceOf[String]
    if ("number" == `type`) {
      new java.lang.Double(jsNumberProperty(pair, "value"))
    } else if ("boolean" == `type`) {
      jsBooleanProperty(pair, "value")
    } else {
      jsProperty(pair, "value")
    }
  }

  private def evaluateJsFunction(script: String, context: XPathContext): SequenceIterator = {
    injectCount += 1
    val fnName = "fnName" + injectCount
    script = script.trim()
    val fnScript = "function " + fnName + "() { return " + script + "; }"
    val item = new JSObjectValue(jsWindow())
    val target = convertToJavaScript(item).asInstanceOf[JavaScriptObject]
    val fs = new ScriptInjector.FromString(fnScript)
    fs.setWindow(target)
    fs.inject()
    val jsArgs = jsArray(0)
    val result = getValueFromTypeValuePair(jsCall(target, fnName, jsArgs))
    convertFromJavaScript(result, context.getConfiguration)
  }

  def iterate(context: XPathContext): SequenceIterator = {
    try {
      if (localName == "window") {
        val item = new JSObjectValue(jsWindow())
        SingletonIterator.makeIterator(item)
      } else if (localName == "eval") {
        val script = argument(0).evaluateAsString(context).toString
        evaluateJsFunction(script, context)
      } else if (localName == "call") {
        val itemVal = argument(0).evaluateItem(context)
        var target = convertToJavaScript(itemVal).asInstanceOf[JavaScriptObject]
        if (target != null) {
          var method = argument(1).evaluateAsString(context).toString
          val lastDot = method.lastIndexOf('.')
          if (lastDot > 0 && lastDot + 1 < method.length) {
            target = getValueFromTypeValuePair(jsSplitPropertyTypeAndValue(target, method.substring(0, 
              lastDot))).asInstanceOf[JavaScriptObject]
            method = method.substring(lastDot + 1)
          }
          val jsArgs = jsArray(argument.length - 2)
          for (i <- 2 until argument.length) {
            val `val` = SequenceExtent.makeSequenceExtent(argument(i).iterate(context))
            jsSetArrayItem(jsArgs, i - 2, convertToJavaScript(`val`))
          }
          try {
            val jsObj = jsCall(target, method, jsArgs)
            val result = getValueFromTypeValuePair(jsObj)
            convertFromJavaScript(result, context.getConfiguration)
          } catch {
            case e: Exception => {
              var doRetry = false
              for (i <- 0 until argument.length - 2 if jsGetArrayItem(jsArgs, i) == null) {
                jsSetArrayItem(jsArgs, i, jsArray(0))
                doRetry = true
              }
              if (doRetry) {
                try {
                  val result = getValueFromTypeValuePair(jsCall(target, method, jsArgs))
                  return convertFromJavaScript(result, context.getConfiguration)
                } catch {
                  case e2: Exception => 
                }
              }
              throw (new XPathException("JavaScriptException in ixsl:call(): Object does not support property or method '" + 
                method + 
                "' with " + 
                (argument.length - 2) + 
                " argument(s)."))
            }
          }
        } else {
          throw (new XPathException("JavaScriptException in ixsl:call(): Call target object is null or undefined"))
        }
      } else if (localName == "get") {
        val itemVal = argument(0).evaluateItem(context)
        val target = convertToJavaScript(itemVal).asInstanceOf[JavaScriptObject]
        if (target != null) {
          val property = argument(1).evaluateAsString(context).toString
          var result: AnyRef = null
          result = getValueFromTypeValuePair(jsSplitPropertyTypeAndValue(target, property))
          convertFromJavaScript(result, context.getConfiguration)
        } else {
          throw (new XPathException("JavaScriptException in ixsl:get(): Get target object is null or undefined"))
        }
      } else if (localName == "page") {
        SingletonIterator.makeIterator(context.getConfiguration.getHostPage)
      } else if (localName == "source") {
        SingletonIterator.makeIterator(context.getController.getSourceNode)
      } else if (localName == "event") {
        val event = context.getController.getUserData("Saxon-CE", "current-event").asInstanceOf[Event]
        SingletonIterator.makeIterator(new JSObjectValue(event))
      } else if (localName == "parse-xml") {
        val data = argument(0).evaluateAsString(context).toString
        convertFromJavaScript(XMLDOM.parseXML(data), context.getConfiguration)
      } else if (localName == "serialize-xml") {
        val arg = argument(0).evaluateItem(context)
        if (arg.isInstanceOf[NodeInfo]) {
          var node: Node = null
          if (arg.isInstanceOf[HTMLNodeWrapper]) {
            node = arg.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode
          } else {
            val writer = new HTMLWriter()
            val pipe = context.getController.makePipelineConfiguration()
            writer.setPipelineConfiguration(pipe)
            val reducer = new NamespaceReducer()
            reducer.setUnderlyingReceiver(writer)
            reducer.setPipelineConfiguration(pipe)
            node = XMLDOM.createDocument(arg.asInstanceOf[NodeInfo].getBaseURI)
            writer.setNode(node)
            Navigator.copy(arg.asInstanceOf[NodeInfo], reducer, 0)
          }
          SingletonIterator.makeIterator(new StringValue(XMLDOM.serializeXML(node)))
        } else {
          SingletonIterator.makeIterator(new StringValue(arg.getStringValue))
        }
      } else {
        logger.warning("No such IXSL function: '" + localName + "' - empty sequence returned")
        EmptyIterator.getInstance
      }
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(this.getSourceLocator)
        throw e
      }
      case e: Exception => {
        val xe = new XPathException("Exception in ixsl:" + localName + "() " + e.getMessage)
        xe.maybeSetLocation(this.getSourceLocator)
        throw xe
      }
    }
  }
}

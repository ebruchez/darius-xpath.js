// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
\package client.net.sf.saxon.ce

import client.net.sf.saxon.ce.Controller.APIcommand
import client.net.sf.saxon.ce.client.HTTPHandler
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper.DocType
import client.net.sf.saxon.ce.dom.Sanitizer
import client.net.sf.saxon.ce.dom.XMLDOM
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.lib.JavaScriptAPIException
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.lib.StandardErrorListener
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.JSObjectPattern
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.Mode
import client.net.sf.saxon.ce.trans.Rule
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import com.google.gwt.core.client.EntryPoint
import com.google.gwt.core.client.JavaScriptException
import com.google.gwt.core.client.JavaScriptObject
import com.google.gwt.dom.client._
import com.google.gwt.http.client.Request
import com.google.gwt.http.client.RequestCallback
import com.google.gwt.http.client.Response
import com.google.gwt.logging.client.LogConfiguration
import com.google.gwt.user.client.DOM
import com.google.gwt.user.client.Event
import com.google.gwt.user.client.EventListener
import com.google.gwt.user.client.Window
import org.timepedia.exporter.client.ExporterUtil
import java.util.ArrayList
import java.util.List
import java.util.logging.Level
import java.util.logging.Logger
import Xslt20ProcessorImpl._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object Xslt20ProcessorImpl {

  private var logger: Logger = Logger.getLogger("XSLT20Processor")

  @BeanProperty
  lazy val scriptsOnLoad = Document.get.getElementsByTagName("s" + "cript")

  def getBodyElement(): Element = {
    var body = Document.get.getElementsByTagName("BODY").getItem(0)
    if (body == null) {
      body = Document.get.getElementsByTagName("body").getItem(0)
    }
    body
  }

  /* native */ def isNonDocNode(obj: JavaScriptObject): Boolean

  /* native */ def bindTemplateToWindowEvent(eventName: String, target: JavaScriptObject): String

  def relayNonDomEvent(name: String, obj: JavaScriptObject, eventArg: JavaScriptObject): Unit = {
    val event = if (eventArg == null) getWindowEvent else eventArg
    Controller.relayNonDomEvent(name, obj, event)
  }

  /* native */ def getWindowEvent(): JavaScriptObject

  def handleException(err: Exception, prefix: String): Unit = {
    if (err.isInstanceOf[JavaScriptAPIException]) {
      return
    }
    var excName: String = null
    prefix = if (prefix == null || prefix.length == 0) "" else " in " + prefix + ":"
    var logException = true
    if (err.isInstanceOf[XPathException]) {
      excName = "XPathException"
      val xe = err.asInstanceOf[XPathException]
      logException = !xe.hasBeenReported()
    } else {
      excName = "Exception " + err.getClass.getName
    }
    val message = excName + prefix + " " + err.getMessage
    if (logException) {
      logger.log(Level.SEVERE, message)
    }
    err.printStackTrace()
    if (SaxonceApi.doThrowJsExceptions()) {
      throw new JavaScriptAPIException("[js] " + message)
    }
  }

  private def eventPropertyMatch(event: Event, matchedRule: Rule): Boolean = {
    val eventProperty = matchedRule.getEventProperty
    if (eventProperty == null) {
      return true
    }
    val all = eventProperty.split("\\s")
    val eventPropertyValue = getEventProperty(event, all(0))
    if (all.length < 2 || eventPropertyValue == null) {
      return true
    }
    var matches = false
    for (i <- 1 until all.length if eventPropertyValue == all(i)) {
      matches = true
      //break
    }
    matches
  }

  private /* native */ def getEventProperty(evt: Event, propName: String): String
}

/**
 * This class represents the XSLT 2.0 processor, which is the top-level object implemented by Saxon-CE
 * Logging is implemented using GWT Logging - documentation at:
 * http://code.google.com/webtoolkit/doc/latest/DevGuideLogging.html
 */
class Xslt20ProcessorImpl extends EntryPoint {

  val config = new Configuration()

  private var registeredForEvents: Boolean = false

  private var principleEventListener: Boolean = false

  var stylesheet: Executable = null

  private var successCallback: JavaScriptObject = null

  var localController: Controller = new Controller(config, true)

  def onModuleLoad(): Unit = {
    if (LogConfiguration.loggingIsEnabled()) {
      SaxonceApi.setAnyExternalErrorHandler()
      LogController.initLogger()
      LogController.addJavaScriptLogHandler()
    }
    logger.log(Level.FINE, "GWT Module Load initated by page: " + Document.get.getTitle)
    if (LogConfiguration.loggingIsEnabled()) {
      val href = Window.Location.getHref
      if (href != null && href.startsWith("file:")) {
        logger.warning("The file:// protocol in use may cause 'permission denied' errors in Saxon-CE - unless the browser's 'strict-origin-policy' has been relaxed.")
      }
    }
    ExporterUtil.exportAll()
    SaxonceApi.register()
    val saxonceLoadCallback = getCallback
    if (saxonceLoadCallback != null) {
      logger.log(Level.FINE, "Executing 'onSaxonceLoad' callback...")
      try {
        executeCallback(saxonceLoadCallback)
      } catch {
        case jse: JavaScriptException => handleException(jse, "onModuleLoad")
        case je: Exception => handleException(je, "onModuleLoad")
      }
    }
    doTransformation()
  }

  private /* native */ def getCallback(): JavaScriptObject

  private /* native */ def executeCallback(callback: JavaScriptObject): Unit

  def getSuccess(): JavaScriptObject = successCallback

  var successOwner: XSLT20Processor = null

  def setSuccess(sFunction: JavaScriptObject, sOwner: XSLT20Processor): Unit = {
    successCallback = sFunction
    successOwner = sOwner
  }

  /**
   * The entry point for transforms initiate on module load,
   * fetches settings from the <code>application/xslt+xml style</code> element
   */
  def doTransformation(): Unit = {
    val logger = Logger.getLogger("Xstl20Processor")
    try {
      val scripts = getScriptsOnLoad
      var sourceURI: String = null
      var styleURI: String = null
      var initialMode: String = null
      var initialTemplate: String = null
      var styleElementExists = false
      for (i <- 0 until scripts.getLength) {
        val `type` = scripts.getItem(i).getAttribute("type")
        if (`type` == "application/xslt+xml") {
          styleElementExists = true
          styleURI = scripts.getItem(i).getAttribute("src")
          sourceURI = scripts.getItem(i).getAttribute("data-source")
          initialMode = scripts.getItem(i).getAttribute("data-initial-mode")
          initialTemplate = scripts.getItem(i).getAttribute("data-initial-template")
          //break
        }
      }
      if (!styleElementExists) {
        logger.info("Saxon-CE API initialised")
        return
      } else if (styleURI == null) {
        throw new XPathException("No XSLT stylesheet reference found")
      }
      var sourceDoc: JavaScriptObject = null
      var absSourceURI: String = null
      if (sourceURI != null && sourceURI.length != 0) {
        val pageHref = Window.Location.getHref
        absSourceURI = new URI(pageHref).resolve(sourceURI).toString
        if (pageHref == absSourceURI) {
          throw new XPathException("Cannot load XML with same URI as the host page")
        }
        sourceDoc = SaxonceApi.createAsyncDoc(absSourceURI)
      } else if (initialTemplate == null) {
        throw new XPathException("No data-source attribute or data-initial-template value found - one is required")
      }
      val absStyleURI = new URI(Window.Location.getHref).resolve(styleURI)
        .toString
      var styleDoc: DocumentInfo = null
      try {
        styleDoc = config.buildDocument(absStyleURI)
      } catch {
        case e: XPathException => {
          val reportURI = if (absSourceURI != null) absSourceURI else styleURI
          throw new XPathException("Failed to load XSLT stylesheet " + reportURI + ": " + 
            e.getMessage)
        }
      }
      config.getDocumentPool.add(styleDoc, absStyleURI)
      val body = getBodyElement
      localController.setInitialMode(initialMode)
      localController.setInitialTemplate(initialTemplate)
      localController.setApiCommand(APIcommand.UPDATE_HTML)
      localController.setTargetNode(Document.get)
      renderXML(sourceDoc, styleDoc, body)
    } catch {
      case err: Exception => logger.log(Level.SEVERE, err.getMessage)
    }
  }

  def continueWithSourceDocument(sourceDoc: DocumentInfo, 
      styleURI: String, 
      initialMode: String, 
      initialTemplate: String): Unit = {
  }

  /**
   * Implementation of XSLT20Processor API - only handles xml dom - must
   * use transformToFragment or updateHTMLDocument for html dom
   * @param sourceDoc
   */
  def updateHTMLDocument(sourceDoc: JavaScriptObject, targetDoc: Document, cmd: APIcommand): Unit = {
    if (targetDoc == null) {
      targetDoc = Document.get
    }
    localController.setApiCommand(cmd)
    localController.setTargetNode(targetDoc)
    renderXML(sourceDoc, importedStylesheet, getBodyElement)
  }

  def transformToDocument(sourceDoc: JavaScriptObject): Node = {
    localController.setTargetNode(XMLDOM.createDocument(localController.getBaseOutputURI))
    localController.setApiCommand(APIcommand.TRANSFORM_TO_DOCUMENT)
    val targetDoc = XMLDOM.createDocument(localController.getBaseOutputURI)
    renderXML(sourceDoc, importedStylesheet, targetDoc)
  }

  def transformToFragment(sourceDoc: JavaScriptObject, ownerDocument: Document): Node = {
    val owner = if (ownerDocument == null) XMLDOM.createDocument(localController.getBaseOutputURI) else ownerDocument
    val targetDocumentFragment = HTMLDocumentWrapper.createDocumentFragment(owner)
    localController.setTargetNode(owner)
    localController.setApiCommand(APIcommand.TRANSFORM_TO_FRAGMENT)
    renderXML(sourceDoc, importedStylesheet, targetDocumentFragment)
  }

  private var importedStylesheet: DocumentInfo = _

  /**
   * Imports new stylesheet and de-registers sinks for specific events from
   * any previous stylesheet for this processor
   * @param doc
   */
  def importStylesheet(doc: JavaScriptObject): Unit = {
    deregisterEventHandlers()
    try {
      importedStylesheet = SaxonceApi.getDocSynchronously(doc, config)
    } catch {
      case e: XPathException => handleException(e, "importStylesheet")
    }
  }

  var fetchedSourceDoc: NodeInfo = _

  var transformInvoked: Boolean = _

  var docFetchRequired: Boolean = _

  def renderXML(inSourceDoc: JavaScriptObject, styleDoc: DocumentInfo, target: com.google.gwt.dom.client.Node): Node = {
    try {
      if (styleDoc == null) {
        throw new Exception("Stylesheet for transform is null")
      }
      docFetchRequired = inSourceDoc != null
      config.setErrorListener(new StandardErrorListener())
      var asyncSourceURI: String = null
      if (docFetchRequired && 
        (localController.getApiCommand == APIcommand.UPDATE_HTML || 
        (successCallback != null))) {
        asyncSourceURI = SaxonceApi.getAsyncUri(inSourceDoc)
        if (asyncSourceURI != null && asyncSourceURI.toLowerCase().startsWith("file:")) {
          asyncSourceURI = null
        }
      }
      fetchedSourceDoc = null
      transformInvoked = false
      if (asyncSourceURI != null) {
        val URI = asyncSourceURI
        val transformTarget = target
        logger.log(Level.FINE, "Aynchronous GET for: " + asyncSourceURI)
        val hr = new HTTPHandler()
        hr.doGet(asyncSourceURI, new RequestCallback() {

          def onError(request: Request, exception: Throwable): Unit = {
            val msg = "HTTP Error " + exception.getMessage + " for URI " + URI
            handleException(new RuntimeException(msg), "onError")
          }

          def onResponseReceived(request: Request, response: Response): Unit = {
            val statusCode = response.getStatusCode
            if (statusCode == 200) {
              Logger.getLogger("ResponseReceived").fine("GET Ok for: " + URI)
              var responseNode: Node = null
              try {
                responseNode = XMLDOM.parseXML(response.getText).asInstanceOf[Node]
              } catch {
                case e: Exception => {
                  handleException(new RuntimeException(e.getMessage), "onResponseReceived")
                  return
                }
              }
              val responseDoc = config.wrapXMLDocument(responseNode, URI)
              val result = invokeTransform(responseDoc, transformTarget)
              hr.setResultNode(result)
            } else if (statusCode < 400) {
            } else {
              val msg = "HTTP Error " + statusCode + " " + response.getStatusText + 
                " for URI " + 
                URI
              handleException(new RuntimeException(msg), "onResponseReceived")
            }
          }
        })
      }
      if (stylesheet == null) {
        if (LogConfiguration.loggingIsEnabled()) {
          LogController.InitializeTraceListener()
        }
        logger.log(Level.FINE, "Compiling Stylesheet...")
        val sheet = new Executable(config)
        sheet.prepare(styleDoc)
        stylesheet = sheet
        logger.log(Level.FINE, "Stylesheet compiled OK")
      }
      if (asyncSourceURI == null && inSourceDoc != null) {
        val nodeType = if (Node.is(inSourceDoc)) inSourceDoc.asInstanceOf[Node].getNodeType else 0
        if (nodeType > 0 && nodeType != Node.DOCUMENT_NODE) {
          val sourceNode = inSourceDoc.asInstanceOf[Node]
          val sourceDoc = sourceNode.getOwnerDocument
          val htmlDoc = new HTMLDocumentWrapper(sourceDoc, sourceDoc.getURL, config, DocType.UNKNOWN)
          fetchedSourceDoc = htmlDoc.wrap(sourceNode)
        } else {
          fetchedSourceDoc = SaxonceApi.getDocSynchronously(inSourceDoc, config)
        }
      }
      if (stylesheet.getStripperRules.isStripping) {
        new Sanitizer(stylesheet.getStripperRules).sanitize(fetchedSourceDoc.asInstanceOf[HTMLDocumentWrapper])
      }
      invokeTransform(fetchedSourceDoc, target)
    } catch {
      case e: Exception => {
        handleException(e, "renderXML")
        null
      }
    }
  }

  private var registeredEventModes: List[Mode] = null

  private var registeredProcessorForNonDomEvents: Boolean = false

  private def registerNonDOMevents(controller: Controller): Unit = {
    for (eventMode <- registeredEventModes) {
      val nonDomRules = eventMode.getVirtualRuleSet
      if (nonDomRules != null) {
        if (!registeredProcessorForNonDomEvents) {
          registeredProcessorForNonDomEvents = true
          Controller.addNonDomEventProcessor(this)
        }
        val eventName = eventMode.getModeName.getLocalName
        for (r <- nonDomRules) {
          var eventTarget: JavaScriptObject = null
          eventTarget = r.getPattern.asInstanceOf[JSObjectPattern].evaluate(controller.newXPathContext())
          bindTemplateToWindowEvent(eventName, eventTarget)
        }
      }
    }
  }

  private def registerEventHandlers(controller: Controller): Unit = {
    if (registeredEventModes != null) {
      return
    }
    val docElement = Document.get.asInstanceOf[AnyRef].asInstanceOf[com.google.gwt.user.client.Element]
    registeredEventModes = controller.getRuleManager.getModesInNamespace(NamespaceConstant.IXSL)
    if (registeredEventModes.size > 0 && !registeredForEvents) {
      registeredForEvents = true
      registerNonDOMevents(controller)
      if (DOM.getEventListener(docElement.asInstanceOf[com.google.gwt.user.client.Element]) == 
        null) {
        principleEventListener = true
        DOM.setEventListener(docElement.asInstanceOf[com.google.gwt.user.client.Element], new EventListener() {

          def onBrowserEvent(event: Event): Unit = {
            val eTarget = event.getEventTarget
            var eventNode: Node = null
            if (Node.is(eTarget)) {
              eventNode = Node.as(eTarget)
            } else {
              eventNode = Node.as(getCorrespondingSVGElement(eTarget))
              if (eventNode == null) {
                return
              }
            }
            bubbleApplyTemplates(eventNode, event)
          }
        })
      } else {
        Controller.addEventProcessor(this)
      }
    }
    for (eventMode <- registeredEventModes) {
      var eventName = eventMode.getModeName.getLocalName
      if (!eventName.startsWith("on")) {
        logger.warning("Event name: '" + eventName + "' is invalid - names should begin with 'on'")
      } else {
        eventName = eventName.substring(2)
      }
      val eventNo = Event.getTypeInt(eventName)
      DOM.sinkEvents(docElement.asInstanceOf[com.google.gwt.user.client.Element], eventNo | 
        DOM.getEventsSunk(docElement.asInstanceOf[com.google.gwt.user.client.Element]))
    }
  }

  def deregisterEventHandlers(): Unit = {
  }

  /**
   * Returns the SVG DOM element associated with an external element that's an SVGElementInstance object
   * E.g. If a <code>use</code> element references a <code>rect</code> element - using xlink:href - then,
   * if the <code>rect</code> object is passed as a parameter, the <code>use</code> element is returned.
   */
  /* native */ def getCorrespondingSVGElement(obj: JavaScriptObject): JavaScriptObject

  /**
   * This invokes a transform, but it may be called either on an async callback or directly
   * We need to ensure this method runs once and only once for a single transform request -
   * It's possible for either the main code branch which performs the compile or the async callback
   * which depends on the compile to makes the call first.
   */
  private def invokeTransform(inDoc: NodeInfo, target: com.google.gwt.dom.client.Node): Node = {
    if (fetchedSourceDoc == null) {
      fetchedSourceDoc = inDoc
    }
    if (transformInvoked || stylesheet == null || (docFetchRequired && fetchedSourceDoc == null)) {
      return null
    }
    transformInvoked = true
    try {
      val controller = stylesheet.newTransformer()
      localController.setSourceNode(fetchedSourceDoc)
      controller.importControllerSettings(localController)
      logger.log(Level.FINE, "Commencing transform type:" + controller.getApiCommand.toString)
      val outResult = controller.transform(fetchedSourceDoc, target)
      logger.log(Level.FINE, "Transform complete")
      localController.importResults(controller)
      registerEventHandlers(controller)
      if (successCallback != null) {
        successOwner.invokeSuccess(successCallback)
      }
      outResult
    } catch {
      case e: Exception => {
        handleException(e, "invokeTransform")
        null
      }
    }
  }

  private def getModeFromEvent(event: Event): Mode = {
    var result: Mode = null
    val mode = "on" + event.getType
    for (m <- registeredEventModes if m.getModeName.getLocalName == mode) {
      result = m
      //break
    }
    result
  }

  def bubbleApplyTemplates(node: Node, event: Event): Unit = {
    if (principleEventListener) {
      Controller.relayEvent(node, event)
    }
    val eventNode = config.getHostPage.asInstanceOf[HTMLDocumentWrapper]
      .wrap(node)
    val bubbleElements = eventNode.iterateAxis(Axis.ANCESTOR, NodeKindTest.ELEMENT)
    val controller = stylesheet.newTransformer()
    try {
      controller.importControllerSettings(localController)
      val ruleContext = controller.newXPathContext()
      val matchedMode = getModeFromEvent(event)
      if (matchedMode == null) {
        return
      }
      var element = eventNode
      while (element != null) {
        val matchedRule = matchedMode.getRule(element, ruleContext)
        if (matchedRule != null && eventPropertyMatch(event, matchedRule)) {
          logger.log(Level.FINER, "Bubble Apply-Templates - Mode: " + matchedMode.getModeName.getLocalName + 
            " Element: " + 
            element.getDisplayName)
          applyEventTemplates(matchedMode.getModeName.getClarkName, element, event, null)
          if (matchedRule.getIxslPreventDefault) {
            event.preventDefault()
          }
          //break
        }
        element = bubbleElements.next().asInstanceOf[NodeInfo]
      }
    } catch {
      case e: Exception => handleException(e, "bubbleApplyTemplates")
    }
  }

  def applyEventTemplates(mode: String, 
      start: NodeInfo, 
      event: JavaScriptObject, 
      `object`: JavaScriptObject): Unit = {
    try {
      if (start == null) {
        start = config.getHostPage
      }
      logger.log(Level.FINER, "OnEvent Apply-Templates - Mode: " + mode + " Event: " + 
        event.toString)
      val controller = stylesheet.newTransformer()
      controller.importControllerSettings(localController)
      controller.setInitialTemplate(null)
      controller.setInitialMode(mode)
      controller.setUserData("Saxon-CE", "current-event", event)
      controller.setUserData("Saxon-CE", "current-object", `object`)
      controller.transform(start, controller.getTargetNode)
    } catch {
      case err: Exception => handleException(err, "mode: '" + mode + "' event: '" + event.toString)
    }
  }

  def getController(): Controller = localController
}

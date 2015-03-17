// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.Controller.APIcommand
import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper.DocType
import client.net.sf.saxon.ce.dom.HTMLNodeWrapper
import client.net.sf.saxon.ce.dom.XMLDOM
import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.FunctionLibrary
import client.net.sf.saxon.ce.functions.ResolveURI
import client.net.sf.saxon.ce.js.JSObjectValue
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.style.StyleElement
import client.net.sf.saxon.ce.sxpath.AbstractStaticContext
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.trans.update.DeleteAction
import client.net.sf.saxon.ce.trans.update.InsertAction
import client.net.sf.saxon.ce.trans.update.PendingUpdateList
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import com.google.gwt.dom.client.Document
import com.google.gwt.dom.client.Node
import com.google.gwt.logging.client.LogConfiguration
import java.util.Iterator
import java.util.logging.Logger
import ResultDocument._
//remove if not needed
import scala.collection.JavaConversions._

object ResultDocument {

  val APPEND_CONTENT = 0

  val REPLACE_CONTENT = 1

  /**
   * Return a valid URI - event if base URI is not set
   */
  def getValidAbsoluteURI(controller: Controller, href: String): String = {
    val baseURI = if ((controller.getBaseOutputURI != null && controller.getBaseOutputURI.length > 0)) controller.getBaseOutputURI else Document.get.getURL
    ResolveURI.makeAbsolute(href, baseURI).toString
  }
}

/**
 * The compiled form of an xsl:result-document element in the stylesheet.
 * <p/>
 * The xsl:result-document element takes an attribute href="filename". The filename will
 * often contain parameters, e.g. {position()} to ensure that a different file is produced
 * for each element instance.
 * <p/>
 * There is a further attribute "format" which determines the format of the
 * output file, it identifies the name of an xsl:output element containing the output
 * format details. In addition, individual serialization properties may be specified as attributes.
 * These are attribute value templates, so they may need to be computed at run-time.
 */
class ResultDocument(var href: Expression, 
    var methodExpression: Expression, 
    baseURI: String, 
    var nsResolver: NamespaceResolver) extends Instruction {

  private var content: Expression = _

  private var logger: Logger = Logger.getLogger("Xstl20Processor")

  adoptChildExpression(href)

  if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
    this.AddTraceProperty("href", href)
  }

  /**
   * Set the expression that constructs the content
   * @param content the expression defining the content of the result document
   */
  def setContentExpression(content: Expression) {
    this.content = content
    adoptChildExpression(content)
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   * @param visitor an expression visitor
   * @return the simplified expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during expression rewriting
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    content = visitor.simplify(content)
    href = visitor.simplify(href)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    content = visitor.typeCheck(content, contextItemType)
    adoptChildExpression(content)
    if (href != null) {
      href = visitor.typeCheck(href, contextItemType)
      adoptChildExpression(href)
    }
    if (methodExpression != null) {
      methodExpression = visitor.typeCheck(methodExpression, contextItemType)
      adoptChildExpression(methodExpression)
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    content = visitor.optimize(content, contextItemType)
    adoptChildExpression(content)
    if (href != null) {
      href = visitor.optimize(href, contextItemType)
      adoptChildExpression(href)
    }
    if (methodExpression != null) {
      methodExpression = visitor.optimize(methodExpression, contextItemType)
      adoptChildExpression(methodExpression)
    }
    this
  }

  def getIntrinsicDependencies(): Int = StaticProperty.HAS_SIDE_EFFECTS

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer) {
    content = doPromotion(content, offer)
    if (href != null) {
      href = doPromotion(href, offer)
    }
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   * @return the static item type of the instruction. This is empty: the result-document instruction
   *         returns nothing.
   */
  def getItemType(): ItemType = EmptySequenceTest.getInstance

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    nonNullChildren(content, href, methodExpression)
  }

  def processLeavingTail(context: XPathContext): TailCall = {
    val controller = context.getController
    val command = controller.getApiCommand
    val c2 = context.newMinorContext()
    var action = APPEND_CONTENT
    if (methodExpression != null) {
      val method = methodExpression.evaluateAsString(context).toString
      var methodQ: StructuredQName = null
      methodQ = if (method.indexOf(':') >= 0) StructuredQName.fromLexicalQName(method, "", nsResolver) else new StructuredQName("", 
        "", method)
      if ("replace-content" == methodQ.getLocalName) {
        action = REPLACE_CONTENT
      }
    }
    var hrefValue: String = null
    if (href != null) {
      hrefValue = href.evaluateAsString(context).toString
    } else if (command == APIcommand.UPDATE_HTML) {
      throw new XPathException("html update - no href value for result-document instruction")
    } else {
      hrefValue = "result" + (controller.getResultDocumentCount + 1)
    }
    var target: NodeInfo = null
    var targetNode: Node = null
    var contextNodeName = ""
    var absURI = ""
    if (command == APIcommand.TRANSFORM_TO_DOCUMENT) {
      absURI = getValidAbsoluteURI(controller, hrefValue)
      targetNode = XMLDOM.createDocument(absURI)
    } else if (command == APIcommand.TRANSFORM_TO_FRAGMENT || command == APIcommand.TRANSFORM_TO_HTML_FRAGMENT) {
      absURI = getValidAbsoluteURI(controller, hrefValue)
      targetNode = HTMLDocumentWrapper.createDocumentFragment(controller.getTargetNode.asInstanceOf[Document])
    } else if (hrefValue.startsWith("#")) {
      hrefValue = hrefValue.substring(1)
      targetNode = controller.getTargetNode.asInstanceOf[Document].getElementById(hrefValue)
    } else if (hrefValue.startsWith("?select=")) {
      val select = hrefValue.substring(8)
      val env = new AbstractStaticContext() {

        def bindVariable(qName: StructuredQName): Expression = return null

        def getNamespaceResolver(): NamespaceResolver = return null

        def getFunctionLibrary(): FunctionLibrary = {
          return controller.getPreparedStylesheet.getFunctionLibrary
        }
      }
      val visitor = ExpressionVisitor.make(env, new Executable(context.getConfiguration))
      env.setConfiguration(context.getConfiguration)
      val container = getSourceLocator.asInstanceOf[StyleElement]
      var expr: Expression = null
      expr = ExpressionTool.make(select, env, container, 0, Token.EOF, getSourceLocator)
      expr = visitor.typeCheck(expr, NodeKindTest.DOCUMENT)
      val c3 = context.newCleanContext()
      val page = controller.getTargetNode.asInstanceOf[Document]
      val cItem = context.getContextItem
      var currentContextItem: NodeInfo = null
      currentContextItem = if (cItem.isInstanceOf[JSObjectValue]) null else cItem.asInstanceOf[NodeInfo]
      var useCurrentContext: Boolean = false
      useCurrentContext = if (currentContextItem == null) false else (currentContextItem.getBaseURI == page.getURL)
      var contextItem: NodeInfo = null
      if (useCurrentContext) {
        contextItem = currentContextItem
        if (LogConfiguration.loggingIsEnabled() && contextItem.getNodeKind == Type.ELEMENT) {
          contextNodeName = contextItem.getDisplayName
        }
      } else {
        contextItem = new HTMLDocumentWrapper(page, page.getURL, context.getConfiguration, DocType.UNKNOWN)
      }
      if (LogConfiguration.loggingIsEnabled()) {
        contextNodeName = (if (contextNodeName == "") "" else " context node: " + contextNodeName)
      }
      c3.setSingletonFocus(contextItem)
      val iter = expr.iterate(c3)
      val resultItem = iter.next()
      if (resultItem == null) {
      } else if (!(resultItem.isInstanceOf[NodeInfo])) {
        throw new XPathException("non-node returned by result-document href: " + hrefValue)
      } else {
        target = resultItem.asInstanceOf[NodeInfo]
        targetNode = target.asInstanceOf[HTMLNodeWrapper].getUnderlyingNode
      }
    } else if (command == APIcommand.UPDATE_HTML) {
      throw new XPathException("expected '?select=' or '#' at start of result-document href, found: " + 
        hrefValue)
    }
    if (targetNode == null) {
      logger.warning("result-document target not found for href: " + hrefValue + 
        contextNodeName)
      return null
    } else {
      logger.fine("processing result-document for href: " + hrefValue + 
        contextNodeName)
    }
    var container: Node = null
    if (command == APIcommand.UPDATE_HTML) {
      container = HTMLDocumentWrapper.createDocumentFragment(controller.getTargetNode.asInstanceOf[Document])
    } else {
      addResultDocument(context, new DocumentURI(absURI), targetNode.asInstanceOf[Document])
      container = targetNode
    }
    val pipe = controller.makePipelineConfiguration()
    val out = controller.openResult(pipe, c2, container, action)
    try {
      content.process(c2)
      out.endDocument()
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
    controller.closeResult(out, c2)
    if (command == APIcommand.UPDATE_HTML) {
      val list = controller.getPendingUpdateList
      if (action == REPLACE_CONTENT && command == APIcommand.UPDATE_HTML) {
        val existingChildren = targetNode.getChildCount
        for (i <- 0 until existingChildren) {
          val child = targetNode.getChild(i)
          list.add(new DeleteAction(child))
        }
      }
      list.add(new InsertAction(container, targetNode, InsertAction.LAST))
    }
    null
  }

  private def addResultDocument(context: XPathContext, documentKey: DocumentURI, doc: Document) {
    val controller = context.getController
    if (controller.getDocumentPool.find(documentKey.toString) != 
      null) {
      dynamicError("Cannot write to a URI that has already been read: " + 
        documentKey.toString, "XTRE1500")
    }
    if (!controller.checkUniqueOutputDestination(documentKey)) {
      dynamicError("Cannot write more than one result document to the same URI: " + 
        documentKey.toString, "XTDE1490")
    } else {
      controller.addToResultDocumentPool(documentKey, doc)
    }
  }
}

// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.SaxonceApi
import client.net.sf.saxon.ce.client.HTTPHandler
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper
import client.net.sf.saxon.ce.dom.Sanitizer
import client.net.sf.saxon.ce.dom.XMLDOM
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.ResolveURI
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.DocumentPool
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.StripSpaceRules
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.IntegerValue
import com.google.gwt.dom.client.Node
import com.google.gwt.http.client.Request
import com.google.gwt.http.client.RequestCallback
import com.google.gwt.http.client.Response
import com.google.gwt.logging.client.LogConfiguration
import com.google.gwt.user.client.Timer
import java.util.Iterator
import java.util.logging.Level
import java.util.logging.Logger
import ScheduleExecution._
//remove if not needed
import scala.collection.JavaConversions._

object ScheduleExecution {

  private var logger: Logger = Logger.getLogger("ScheduleExecution")
}

/**
 * The compiled form of an ixsl:schedule-action instruction in the stylesheet.
 */
class ScheduleExecution(var call: CallTemplate, var wait: Expression, var href: Expression)
    extends Instruction {

  private var staticBaseURI: String = _

  adoptChildExpression(call)

  adoptChildExpression(wait)

  adoptChildExpression(href)

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   *
   * @param visitor an expression visitor
   * @return the simplified expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during expression rewriting
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    call = visitor.simplify(call).asInstanceOf[CallTemplate]
    wait = visitor.simplify(wait)
    href = visitor.simplify(href)
    staticBaseURI = visitor.getStaticContext.getBaseURI
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    call = visitor.typeCheck(call, contextItemType).asInstanceOf[CallTemplate]
    wait = visitor.typeCheck(wait, contextItemType)
    href = visitor.typeCheck(href, contextItemType)
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    call = visitor.optimize(call, contextItemType).asInstanceOf[CallTemplate]
    wait = visitor.optimize(wait, contextItemType)
    href = visitor.optimize(href, contextItemType)
    this
  }

  def getIntrinsicDependencies(): Int = StaticProperty.HAS_SIDE_EFFECTS

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   *
   * @param offer The type of rewrite being offered
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    call = doPromotion(call, offer).asInstanceOf[CallTemplate]
    wait = doPromotion(wait, offer)
    href = doPromotion(href, offer)
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   *
   * @return the static item type of the instruction. This is empty: the set-attribute instruction
   *         returns nothing.
   */
  def getItemType(): ItemType = EmptySequenceTest.getInstance

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(call, wait, href)

  def processLeavingTail(context: XPathContext): TailCall = {
    var time = 1
    var hrefVal: String = null
    if (href != null) {
      hrefVal = href.evaluateAsString(context).toString
    } else if (wait != null) {
      time = wait.evaluateItem(context).asInstanceOf[IntegerValue]
        .intValue()
    }
    val pack = call.processLeavingTail(context).asInstanceOf[CallTemplate.CallTemplatePackage]
    if (href != null) {
      var abs: URI = null
      abs = ResolveURI.makeAbsolute(hrefVal, staticBaseURI)
      val uri = abs.toString
      val pool = context.getController.getDocumentPool
      val existingDoc = pool.find(uri)
      if (existingDoc != null) {
        val c2 = context.newMinorContext()
        c2.setSingletonFocus(existingDoc)
        pack.setEvaluationContext(c2)
        var tc = pack.processLeavingTail()
        while (tc != null) {
          tc = tc.processLeavingTail()
        }
        context.getController.getPendingUpdateList.apply(context)
      } else {
        logger.log(Level.FINE, "Aynchronous GET for: " + abs)
        val hr = new HTTPHandler()
        hr.doGet(uri, new RequestCallback() {

          def onError(request: Request, exception: Throwable): Unit = {
            val msg = "HTTP Error " + exception.getMessage + " for URI " + uri
            logger.log(Level.SEVERE, msg)
            if (SaxonceApi.doThrowJsExceptions()) {
              throw new RuntimeException(exception.getMessage)
            }
          }

          def onResponseReceived(request: Request, response: Response): Unit = {
            val statusCode = response.getStatusCode
            if (statusCode == 200) {
              Logger.getLogger("ResponseReceived").fine("GET Ok for: " + uri)
              var responseNode: Node = null
              try {
                responseNode = XMLDOM.parseXML(response.getText).asInstanceOf[Node]
              } catch {
                case e: Exception ⇒ {
                  logger.log(Level.SEVERE, "Failed to parse XML: " + e.getMessage)
                  if (SaxonceApi.doThrowJsExceptions()) {
                    throw new RuntimeException(e.getMessage)
                  }
                  return
                }
              }
              val doc = context.getConfiguration.wrapXMLDocument(responseNode, uri)
              if (doc.isInstanceOf[HTMLDocumentWrapper]) {
                val rules = context.getController.getExecutable.getStripperRules
                if (rules.isStripping) {
                  new Sanitizer(rules).sanitize(doc.asInstanceOf[HTMLDocumentWrapper])
                }
              }
              pool.add(doc, uri)
              val c2 = context.newMinorContext()
              c2.setSingletonFocus(doc)
              pack.setEvaluationContext(c2)
              try {
                var tc = pack.processLeavingTail()
                while (tc != null) {
                  tc = tc.processLeavingTail()
                }
                context.getController.getPendingUpdateList.apply(context)
              } catch {
                case e: XPathException ⇒ {
                  logger.log(Level.SEVERE, "In async document processing: " + e.getMessage)
                  if (SaxonceApi.doThrowJsExceptions()) {
                    throw new RuntimeException(e.getMessage)
                  }
                }
              }
            } else if (statusCode < 400) {
            } else {
              val msg = "HTTP Error " + statusCode + " " + response.getStatusText + 
                " for URI " + 
                uri
              logger.log(Level.SEVERE, msg)
            }
          }
        })
      }
    } else {
      val t = new Timer() {

        def run(): Unit = {
          var success = false
          logger.fine("processing ixsl:schedule-action")
          if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
            LogController.openTraceListener()
          }
          try {
            var tc = pack.processLeavingTail()
            while (tc != null) {
              tc = tc.processLeavingTail()
            }
            context.getController.getPendingUpdateList.apply(context)
            success = true
          } catch {
            case err: Exception ⇒ {
              logger.log(Level.SEVERE, "In delayed event: " + err.getMessage)
              if (SaxonceApi.doThrowJsExceptions()) {
                throw new RuntimeException(err.getMessage)
              }
            }
          }
          if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
            LogController.closeTraceListener(success)
          }
        }
      }
      t.schedule(time)
    }
    null
  }
}

/**
 * Copyright 2015 Orbeon, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package client.net.sf.saxon.ce.orbeon

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions._
import client.net.sf.saxon.ce.om.{Item, Sequence}
import client.net.sf.saxon.ce.sxpath.SimpleContainer
import client.net.sf.saxon.ce.{value ⇒ svalue}
import org.scalajs.dom.raw
import org.scalajs.jquery.{JQuery, JQueryEventObject, jQuery}
import rx._
import rx.ops._

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success, Try}

@JSExport
object XPathProcessor {

  case class CompiledExpression(config: Configuration, expr: Expression, slots: Int)

  val DebounceDelay = 200.millis

  implicit val scheduler = new DomScheduler

  private object XMLParsing {

    val SearchDOMErrorExpression = compileExpression(
      """
        (
          (: Firefox places a root `parsererror` element :)
          for $t in /*:parsererror[namespace-uri() = 'http://www.mozilla.org/newlayout/xml/parsererror.xml']/text()
            return substring-before(substring-after($t, 'XML Parsing Error: '), 'Location:'),
          (: Chrome/Safari places a nested `parsererror` element :)
          /*/*:parsererror/*:div/string()
        )[1]
      """
    )

    val DOMParser = new raw.DOMParser

    // DOMParser has insane error handling. Instead of throwing an exception with relevant information, it returns an
    // error document, or a document with an error element. The result is different between Firefox and Chrome. I don't
    // know whether this works with IE. The standard says to follow the Firefox behavior:
    // http://www.w3.org/TR/DOM-Parsing/#methods
    def findXMLError(doc: raw.Document) = {
      SearchDOMErrorExpression flatMap {
        case compiledExpr @ CompiledExpression(config, _, _) ⇒
          runExpression(compiledExpr, new HTMLDocumentWrapper(doc, null, config))
      } map {
        case List(errorString: svalue.StringValue) ⇒ Some(errorString.getStringValue)
        case _                                     ⇒ None
      }
    }

    def parseXML(s: String): Try[raw.Document] = {

      val newDocument = DOMParser.parseFromString(s, "application/xml")

      findXMLError(newDocument) match {
        case Success(Some(errorMessage)) ⇒ Failure(new Exception(errorMessage))
        case Success(None)               ⇒ Success(newDocument)
        case Failure(t)                  ⇒ Failure(t)
      }
    }
  }

  private object UI {

    def exprInput = jQuery("#expression-textarea")
    def xmlInput  = jQuery("#input-textarea")
    def results   = jQuery("#result-list")

    def exprAlert(field: JQuery) = field.parent().find(".alert")

    def toggleAlert(field: JQuery, text: Option[String]) = {
      field.parent().toggleClass("has-error", text.isDefined)
      field.parent().toggleClass("has-success", text.isEmpty)
      UI.exprAlert(field).text(text.getOrElse(""))
      UI.exprAlert(field).toggleClass("hidden", text.isEmpty)
    }

    def keyChange(getValue: ⇒ String, rx: Var[String])(x: JQueryEventObject) = {
      val newValue = getValue
      if (rx() != newValue)
        rx() = newValue
    }
  }

  @JSExport
  def initialize(): Unit = {

    // Model
    val exprStringVar         = Var(UI.exprInput.value.toString)
    val xmlStringVar          = Var(UI.xmlInput.value.toString)

    val debouncedExprStringRx = exprStringVar.debounce(DebounceDelay)
    val debouncedXmlStringRx  = xmlStringVar.debounce(DebounceDelay)

    val compiledExprRx        = Rx(compileExpression(debouncedExprStringRx()))
    val parsedXMLRx           = Rx(XMLParsing.parseXML(debouncedXmlStringRx()))

    val resultRx = Rx {
      for {
        compiledExpr @ CompiledExpression(config, _, _) ← compiledExprRx()
        parsedXML    ← parsedXMLRx()
        result       ← runExpression(compiledExpr, new HTMLDocumentWrapper(parsedXML, null, config))
      } yield
        result
    }

    // Alerts
    compiledExprRx foreach {
      case Success(_) ⇒ UI.toggleAlert(UI.exprInput, None)
      case Failure(t) ⇒ UI.toggleAlert(UI.exprInput, Some(t.getMessage))
    }

    parsedXMLRx foreach {
      case Success(_) ⇒ UI.toggleAlert(UI.xmlInput, None)
      case Failure(t) ⇒ UI.toggleAlert(UI.xmlInput, Some(t.getMessage))
    }

    resultRx foreach {
      case Success(_) ⇒ UI.toggleAlert(UI.results, None)
      case Failure(t) ⇒ UI.toggleAlert(UI.results, Some(t.getMessage))
    }

    // Result
    resultRx foreach { result ⇒
      result foreach { items ⇒
        UI.results.children("li").detach()
        for (item ← items) {
          val itemString = item.getStringValue
          UI.results.append(s"""<li class="list-group-item">$itemString</li>""")
        }
      }
    }

    UI.exprInput.keyup(UI.keyChange(UI.exprInput.value.toString, exprStringVar) _)
    UI.xmlInput.keyup(UI.keyChange(UI.xmlInput.value.toString, xmlStringVar) _)
  }

  val ns = Map(
    "xs" → "http://www.w3.org/2001/XMLSchema"
  )

  def compileExpression(expression: String): Try[CompiledExpression] = Try {

    val library = SystemFunctionLibrary.getSystemFunctionLibrary(StandardFunction.CORE)

    val executable    = new Executable
    val container     = new SimpleContainer(executable)

    val config        = new Configuration
    val staticContext = new ShareableXPathStaticContext(config, ns, library)

    var expr = ExpressionTool.make(expression, staticContext, container, 0, Token.EOF, container)

    expr.setContainer(container)
    val visitor = ExpressionVisitor.make(staticContext, executable)
    expr = visitor.typeCheck(expr, Type.ITEM_TYPE)
    expr = visitor.optimize(expr, Type.ITEM_TYPE)

    CompiledExpression(config, expr, ExpressionTool.allocateSlots(expr, 0))
  }

  // Saxon StringValue doesn't support equals()
  private class StringValueWithEquals(cs: CharSequence) extends svalue.StringValue(cs) {
    override def equals(other: Any): Boolean = other match {
      case s: svalue.StringValue ⇒ codepointEquals(s)
      case o ⇒ false
    }
  }

  private def convertItem(item: Item) = item match {
    case s: svalue.StringValue ⇒ new StringValueWithEquals(s.getPrimitiveStringValue)
    case i ⇒ i
  }

  def runExpression(compiledExpr: CompiledExpression, contextItem: Item): Try[immutable.List[Item]] = Try {

    val CompiledExpression(config, expr, slots) = compiledExpr

    val controller = new Controller(config, expr.getExecutable)

    val xpc = new XPathContext(controller)
    xpc.setStackFrame(slots, new Array[Sequence](slots))
    xpc.setSingletonFocus(contextItem)

    val seqIt = expr.iterate(xpc)

    scala.collection.Iterator.continually(seqIt.next()).takeWhile(_ ne null).map(convertItem).toList
  }
}

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
package org.orbeon.darius.xpath.demo

import org.orbeon.darius.xml.api.API
import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.event.PipelineConfiguration
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.functions._
import org.orbeon.darius.xpath.om.{DocumentInfo, Item, Sequence}
import org.orbeon.darius.xpath.orbeon.{Configuration, Controller, Executable, LinkedTreeDocumentHandler, ShareableXPathStaticContext}
import org.orbeon.darius.xpath.sxpath.SimpleContainer
import org.orbeon.darius.xpath.{value ⇒ svalue}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLScriptElement
import org.scalajs.jquery.{JQuery, JQueryEventObject, jQuery}
import rx._
import rx.ops._
import upickle._

import scala.collection.{immutable ⇒ i}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global ⇒ g}
import scala.util.{Failure, Success, Try}

sealed trait Message
case class ExprReq  (expr: String)                          extends Message
case class ExprRes  (message: Option[String])               extends Message
case class XMLReq   (expr: String)                          extends Message
case class XMLRes   (message: Option[String])               extends Message
case class ResultRes(items: Either[String, i.List[String]]) extends Message

object Launcher extends js.JSApp {
  def main(): Unit =
    if (js.isUndefined(g.document))
      XMLWorker.main()
  else
      XPathDemo.main()
}

object XMLWorker {

  import DariusXMLParsing._
  import XPathProcessor._

  val GlobalConfiguration = new Configuration

  def postResponse(m: Message) = g.postMessage(write(m))

  // What we receive from the client (initially empty)
  val exprStringVar  = Var[Option[String]](None)
  val xmlStringVar   = Var[Option[String]](None)

  val compiledExprRx = Rx(exprStringVar() map (compileExpression(_, GlobalConfiguration)))
  val parsedXMLRx    = Rx(xmlStringVar()  map (parseString(_, GlobalConfiguration)))

  compiledExprRx foreach {
    case Some(Success(_)) ⇒ postResponse(ExprRes(None))
    case Some(Failure(t)) ⇒ postResponse(ExprRes(Option(t.getMessage)))
    case None             ⇒
  }

  parsedXMLRx foreach {
    case Some(Success(_)) ⇒ postResponse(XMLRes(None))
    case Some(Failure(t)) ⇒ postResponse(XMLRes(Option(t.getMessage)))
    case None             ⇒
  }

  val resultRx = Rx {
    for {
      tryCompiledExpr ← compiledExprRx()
      tryParsedXML    ← parsedXMLRx()
    } yield {
      for {
        compiledExpr  ← tryCompiledExpr
        parsedXML     ← tryParsedXML
        result        ← runExpression(compiledExpr, parsedXML)
      } yield
        result
    }
  }

  resultRx foreach {
    case Some(Success(items)) ⇒ postResponse(ResultRes(Right(items map (_.getStringValue))))
    case Some(Failure(t))     ⇒ postResponse(ResultRes(Left(t.getMessage)))
    case None                 ⇒
  }

  def main(): Unit = {
    g.onmessage = (e: raw.MessageEvent) ⇒ read[Message](e.data.asInstanceOf[String]) match {
      case ExprReq(expr) ⇒ exprStringVar() = Option(expr)
      case XMLReq(xml)   ⇒ xmlStringVar()  = Option(xml)
      case _             ⇒
    }
  }
}

object XPathDemo {

  val DebounceDelay = 200.millis

  implicit val scheduler = new DomScheduler

  object UI {

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

  def main(): Unit = {

    val worker = new raw.Worker(jQuery("#scalajs-combined-script")(0).asInstanceOf[HTMLScriptElement].src)

    def postRequest(m: Message) = worker.postMessage(write(m))

    // Model
    val exprStringVar         = Var(UI.exprInput.value.toString)
    val xmlStringVar          = Var(UI.xmlInput.value.toString)

    val debouncedExprStringRx = exprStringVar.debounce(DebounceDelay)
    val debouncedXmlStringRx  = xmlStringVar.debounce(DebounceDelay)

    debouncedExprStringRx foreach { value ⇒
      postRequest(ExprReq(value))
    }

    debouncedXmlStringRx foreach { value ⇒
      postRequest(XMLReq(value))
    }

    val resultVar = Var[Option[Either[String, i.List[String]]]](None)

    // Result
    resultVar foreach { result ⇒
      result foreach {
        case Right(items) ⇒
          UI.toggleAlert(UI.results, None)
          UI.results.children("li").detach()
          for (item ← items) {
            UI.results.append(s"""<li class="list-group-item">$item</li>""")
          }
        case Left(message) ⇒
          UI.toggleAlert(UI.results, Option(message))
      }
    }

    // Events
    UI.exprInput.keyup(UI.keyChange(UI.exprInput.value.toString, exprStringVar) _)
    UI.xmlInput.keyup(UI.keyChange(UI.xmlInput.value.toString, xmlStringVar) _)

    // Message handler
    worker.onmessage = (e: js.Any) ⇒ {

      val serializedMessage = e.asInstanceOf[raw.MessageEvent].data.asInstanceOf[String]

      read[Message](serializedMessage) match {
        case ExprRes(message) ⇒ UI.toggleAlert(UI.exprInput, message)
        case XMLRes(message)  ⇒ UI.toggleAlert(UI.xmlInput, message)
        case ResultRes(items) ⇒ resultVar() = Some(items)
        case _                ⇒
      }
    }
  }
}

object XPathProcessor {

  case class CompiledExpression(config: Configuration, expr: Expression, slots: Int)

  val DefaultNamespaces = Map(
    "xs" → "http://www.w3.org/2001/XMLSchema"
  )

  def compileExpression(expression: String, config: Configuration): Try[CompiledExpression] = Try {

    println(s"compiling: `$expression`")

    val library = SystemFunctionLibrary.getSystemFunctionLibrary(StandardFunction.CORE)

    val executable    = new Executable
    val container     = new SimpleContainer(executable)
    val staticContext = new ShareableXPathStaticContext(config, DefaultNamespaces, library)

    var expr = ExpressionTool.make(expression, staticContext, container, 0, Token.EOF, container)

    expr.setContainer(container)
    val visitor = ExpressionVisitor.make(staticContext, executable)

    println(s"initial expression: `$expr`")

    expr = visitor.typeCheck(expr, Type.ITEM_TYPE)
    expr = visitor.optimize(expr, Type.ITEM_TYPE)

    println(s"type-checked and optimized expression: `$expr`")

    CompiledExpression(config, expr, ExpressionTool.allocateSlots(expr, 0))
  }

  // Saxon StringValue doesn't support equals()
  class StringValueWithEquals(cs: CharSequence) extends svalue.StringValue(cs) {
    override def equals(other: Any): Boolean = other match {
      case s: svalue.StringValue ⇒ codepointEquals(s)
      case o ⇒ false
    }
  }

  def convertItem(item: Item) = item match {
    case s: svalue.StringValue ⇒ new StringValueWithEquals(s.getPrimitiveStringValue)
    case i ⇒ i
  }

  def runExpression(compiledExpr: CompiledExpression, contextItem: Item): Try[i.List[Item]] = Try {

    println(s"running expression: `${compiledExpr.expr}`")

    val CompiledExpression(config, expr, slots) = compiledExpr

    val controller = new Controller(config, expr.getExecutable)

    val xpc = new XPathContext(controller)
    xpc.setStackFrame(slots, new Array[Sequence](slots))
    xpc.setSingletonFocus(contextItem)

    val seqIt = expr.iterate(xpc)

    val res = scala.collection.Iterator.continually(seqIt.next()).takeWhile(_ ne null).map(convertItem).toList
    println(s"done running expression (${res.size} items)")
    res
  }
}

object DariusXMLParsing {

  def parseString(s: String, config: Configuration): Try[DocumentInfo] = Try {

    println(s"parsing XML")

    val pipelineConfig = new PipelineConfiguration()
    pipelineConfig.setConfiguration(config)
    val builder = new LinkedTreeDocumentHandler(pipelineConfig)
    API.parseString(s, builder)
    val res = builder.result.asInstanceOf[DocumentInfo]
    println(s"done parsing XML")
    res
  }
}
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
import org.scalajs.dom.raw
import org.scalajs.jquery.{JQuery, JQueryEventObject, jQuery}

import scala.collection.immutable
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success, Try}

@JSExport
object XPathProcessor {

  def main(args: Array[String]): Unit = initialize()

  case class CompiledExpression(config: Configuration, expr: Expression, slots: Int)

  object UI {
    def exprInput = jQuery("#expression-textarea")
    def xmlInput  = jQuery("#input-textarea")
    def results   = jQuery("#result-list")
//    def results   = jQuery("#result-textarea")
//    def runButton = jQuery(".try-button")
    
    def exprAlert(field: JQuery) = field.parent().find(".alert")
  }

  @JSExport
  def initialize(): Unit = {

    def compileAndRunExpression(): Unit = {

      val currentExpr = UI.exprInput.value.toString

      def parseXML =
        Try(jQuery.parseXML(UI.xmlInput.value.toString).asInstanceOf[raw.Document])

      def toggleAlert(field: JQuery, text: Option[String]) = {
        field.parent().toggleClass("has-error", text.isDefined)
        field.parent().toggleClass("has-success", text.isEmpty)
        UI.exprAlert(field).text(text.getOrElse(""))
        UI.exprAlert(field).toggleClass("hidden", text.isEmpty)
      }

      compileExpression(currentExpr) flatMap {
        case compiledExpr @ CompiledExpression(config, _, _) =>
          parseXML match {
            case Success(xml) =>
              toggleAlert(UI.xmlInput, None)
              runExpression(compiledExpr, new HTMLDocumentWrapper(xml, null, config))
            case Failure(t) =>
              toggleAlert(UI.xmlInput, Some("Error parsing XML document"))
              Failure(t)
          }
      } match {
        case Success(Nil) =>
//          UI.results.value("")
          UI.results.children("li").detach()
          toggleAlert(UI.exprInput, None)
        case Success(items) =>
//          UI.results.value(item.getStringValue())
          UI.results.children("li").detach()
          for (item <- items) {
            val itemString = item.getStringValue()
            UI.results.append(s"""<li class="list-group-item">$itemString</li>""")
          }

          toggleAlert(UI.exprInput, None)
        case Failure(t) =>
          toggleAlert(UI.exprInput, Some(t.getMessage))
      }
    }

    def keyChange(container: Array[Option[String]], getValue: => String)(x: JQueryEventObject) = {
      val newValue = getValue
      if (Some(newValue) != container(0)) {
        compileAndRunExpression()
        container(0) = Some(newValue)
      }
    }

    // Event handlers
//    UI.runButton.click(compileAndRunExpression _)
    UI.exprInput.keyup(keyChange(Array(None), UI.exprInput.value.toString) _)
    UI.xmlInput.keyup(keyChange(Array(None), UI.xmlInput.value.toString) _)

    // Initial compilation
    compileAndRunExpression()
  }

  val ns = Map(
    "xs" -> "http://www.w3.org/2001/XMLSchema"
  )

//  "for $foo in //div return concat('name: ', name($foo))",
//  "for $foo in //div return (if ($foo castable as xs:decimal) then xs:decimal($foo) else ())",
//  "for $foo in //div return $foo[@id = 'my-id'][. castable as xs:decimal]/xs:decimal(.)"

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

  def runExpression(compiledExpr: CompiledExpression, contextItem: Item): Try[immutable.List[Item]] = Try {

    val CompiledExpression(config, expr, slots) = compiledExpr

    val controller = new Controller(config, expr.getExecutable())

    val xpc = new XPathContext(controller)
    xpc.setStackFrame(slots, new Array[Sequence](slots))

    xpc.setSingletonFocus(contextItem)

    val seqIt = expr.iterate(xpc)

    scala.collection.Iterator.continually(seqIt.next()).takeWhile(_ ne null).toList
  }
}

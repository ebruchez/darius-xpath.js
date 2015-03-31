// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.event.ProxyReceiver
import client.net.sf.saxon.ce.event.StartTagBuffer
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.SourceLocator
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.DateTimeValue
import java.util.Stack
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This is a filter inserted into the input pipeline for processing stylesheet modules, whose
 * task is to evaluate use-when expressions and discard those parts of the stylesheet module
 * for which the use-when attribute evaluates to false.
 */
class UseWhenFilter(var startTag: StartTagBuffer, resolver: NamespaceResolver)
    extends ProxyReceiver {

  private var nsResolver: NamespaceResolver = resolver

  private var depthOfHole: Int = 0

  private var emptyStylesheetElement: Boolean = false

  private var defaultNamespaceStack: Stack[String] = new Stack[String]()

  private var currentDateTime: DateTimeValue = DateTimeValue.getCurrentDateTime(null)

  /**
   * Start of document
   */
  def open(): Unit = {
    nextReceiver.open()
  }

  /**
   * Notify the start of an element.
   *
   * @param qName    integer code identifying the name of the element within the name pool.
   * @param properties  bit-significant properties of the element node
   */
  def startElement(qName: StructuredQName, properties: Int): Unit = {
    defaultNamespaceStack.push(startTag.getAttribute("", "xpath-default-namespace"))
    if (emptyStylesheetElement) {
      depthOfHole += 1
      return
    }
    if (depthOfHole == 0) {
      var useWhen: String = null
      val uriCode = qName.getNamespaceURI
      useWhen = if (uriCode == NamespaceConstant.XSLT) startTag.getAttribute("", "use-when") else startTag.getAttribute(NamespaceConstant.XSLT, 
        "use-when")
      if (useWhen != null) {
        var expr: Expression = null
        try {
          val loc = new SourceLocator() {

            def getSystemId(): String = return UseWhenFilter.this.getSystemId

            def getLocation(): String = {
              return "use-when expression in " + getSystemId
            }
          }
          val staticContext = new UseWhenStaticContext(getConfiguration, nsResolver, loc)
          expr = prepareUseWhen(useWhen, staticContext, loc)
          val b = evaluateUseWhen(expr, staticContext)
          if (!b) {
            val local = qName.getLocalName
            if (qName.getNamespaceURI == NamespaceConstant.XSLT && (local == "stylesheet" || local == "transform")) {
              emptyStylesheetElement = true
            } else {
              depthOfHole = 1
              return
            }
          }
        } catch {
          case e: XPathException => {
            val err = new XPathException("Error in use-when expression. " + e.getMessage)
            err.setLocator(expr.getSourceLocator)
            err.setErrorCodeQName(e.getErrorCodeQName)
            getPipelineConfiguration.getErrorListener.error(err)
            err.setHasBeenReported(true)
            throw err
          }
        }
      }
      nextReceiver.startElement(qName, properties)
    } else {
      depthOfHole += 1
    }
  }

  /**
   * Notify a namespace. Namespaces are notified <b>after</b> the startElement event, and before
   * any children for the element. The namespaces that are reported are only required
   * to include those that are different from the parent element; however, duplicates may be reported.
   * A namespace must not conflict with any namespaces already used for element or attribute names.
   *
   * @param nsBinding an integer: the top half is a prefix code, the bottom half a URI code.
   *                      These may be translated into an actual prefix and URI using the name pool. A prefix code of
   *                      zero represents the empty prefix (that is, the default namespace). A URI code of zero represents
   *                      a URI of "", that is, a namespace undeclaration.
   * @throws IllegalStateException: attempt to output a namespace when there is no open element
   *                                start tag
   */
  def namespace(nsBinding: NamespaceBinding, properties: Int): Unit = {
    if (depthOfHole == 0) {
      nextReceiver.namespace(nsBinding, properties)
    }
  }

  /**
   * Notify an attribute. Attributes are notified after the startElement event, and before any
   * children. Namespaces and attributes may be intermingled.
   *
   *
   * @param nameCode   The name of the attribute, as held in the name pool
   * @throws IllegalStateException: attempt to output an attribute when there is no open element
   *                                start tag
   */
  def attribute(nameCode: StructuredQName, value: CharSequence): Unit = {
    if (depthOfHole == 0) {
      nextReceiver.attribute(nameCode, value)
    }
  }

  /**
   * Notify the start of the content, that is, the completion of all attributes and namespaces.
   * Note that the initial receiver of output from XSLT instructions will not receive this event,
   * it has to detect it itself. Note that this event is reported for every element even if it has
   * no attributes, no namespaces, and no content.
   */
  def startContent(): Unit = {
    if (depthOfHole == 0) {
      nextReceiver.startContent()
    }
  }

  /**
   * End of element
   */
  def endElement(): Unit = {
    defaultNamespaceStack.pop()
    if (depthOfHole > 0) {
      depthOfHole -= 1
    } else {
      nextReceiver.endElement()
    }
  }

  /**
   * Character data
   */
  def characters(chars: CharSequence): Unit = {
    if (depthOfHole == 0) {
      nextReceiver.characters(chars)
    }
  }

  /**
   * Processing Instruction
   */
  def processingInstruction(target: String, data: CharSequence): Unit = {
  }

  /**
   * Output a comment
   */
  def comment(chars: CharSequence): Unit = {
  }

  /**
   * Evaluate a use-when attribute
   * @param expression the expression to be evaluated
   * @param sourceLocator identifies the location of the expression in case error need to be reported
   * @return the effective boolean value of the result of evaluating the expression
   */
  def prepareUseWhen(expression: String, staticContext: UseWhenStaticContext, sourceLocator: SourceLocator): Expression = {
    staticContext.setBaseURI(sourceLocator.getSystemId)
    staticContext.setDefaultElementNamespace(NamespaceConstant.NULL)
    var i = defaultNamespaceStack.size - 1
    while (i >= 0) {
      val uri = defaultNamespaceStack.get(i).asInstanceOf[String]
      if (uri != null) {
        staticContext.setDefaultElementNamespace(uri)
        //break
      }
      i -= 1
    }
    val expr = ExpressionTool.make(expression, staticContext, staticContext, 0, Token.EOF, sourceLocator)
    expr.setContainer(staticContext)
    expr
  }

  def evaluateUseWhen(expr: Expression, staticContext: UseWhenStaticContext): Boolean = {
    val contextItemType = Type.ITEM_TYPE
    val visitor = ExpressionVisitor.make(staticContext, staticContext.getExecutable)
    expr = visitor.typeCheck(expr, contextItemType)
    val slots = ExpressionTool.allocateSlots(expr, 0)
    val controller = new Controller(getConfiguration)
    controller.setCurrentDateTime(currentDateTime)
    var dynamicContext = controller.newXPathContext()
    dynamicContext = dynamicContext.newCleanContext()
    dynamicContext.openStackFrame(slots)
    expr.effectiveBooleanValue(dynamicContext)
  }
}

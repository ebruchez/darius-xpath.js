// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.event.Builder
import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.StringJoin
import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.StringValue
import client.net.sf.saxon.ce.value.TextFragmentValue
import client.net.sf.saxon.ce.value.UntypedAtomicValue
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction to create a document node. This corresponds to the xsl:document-node
 * instruction in XSLT. It is also used to support the document node constructor
 * expression in XQuery, and is generated implicitly within an xsl:variable
 * that constructs a temporary tree.
 *
 * <p>Conceptually it represents an XSLT instruction xsl:document-node,
 * with no attributes, whose content is a complex content constructor for the
 * children of the document node.</p>
 */
class DocumentInstr(@BooleanBeanProperty var textOnly: Boolean, var constantText: String, baseURI: String)
    extends ParentNodeConstructor {

  setBaseURI(baseURI)

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered. For instructions this is the process() method.
   */
  def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   *
   * @return the simplified expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if an error is discovered during expression rewriting
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = super.simplify(visitor)

  /**
   * In the case of a text-only instruction (xsl:variable containing a text node or one or more xsl:value-of
   * instructions), return an expression that evaluates to the textual content as an instance of xs:untypedAtomic
   * @param env the static evaluation context
   * @return an expression that evaluates to the textual content
   */
  def getStringValueExpression(env: StaticContext): Expression = {
    if (textOnly) {
      if (constantText != null) {
        new StringLiteral(new UntypedAtomicValue(constantText))
      } else if (content.isInstanceOf[ValueOf]) {
        content.asInstanceOf[ValueOf].convertToCastAsString()
      } else {
        val fn = SystemFunction.makeSystemFunction("string-join", Array(content, new StringLiteral(StringValue.EMPTY_STRING))).asInstanceOf[StringJoin]
        val cast = new CastExpression(fn, AtomicType.UNTYPED_ATOMIC, false)
        ExpressionTool.copyLocationInfo(this, cast)
        cast
      }
    } else {
      throw new AssertionError("getStringValueExpression() called on non-text-only document instruction")
    }
  }

  /**
   * Get the item type
   * @return the in
   */
  def getItemType(): ItemType = NodeKindTest.DOCUMENT

  def processLeavingTail(context: XPathContext): TailCall = {
    val out = context.getReceiver
    out.startDocument()
    content.process(context)
    out.endDocument()
    null
  }

  /**
   * Evaluate as an expression.
   */
  def evaluateItem(context: XPathContext): Item = {
    val controller = context.getController
    var root: DocumentInfo = null
    if (textOnly) {
      var textValue: CharSequence = null
      if (constantText != null) {
        textValue = constantText
      } else {
        val sb = new FastStringBuffer(FastStringBuffer.SMALL)
        val iter = content.iterate(context)
        while (true) {
          val item = iter.next()
          if (item == null) //break
          sb.append(item.getStringValue)
        }
        textValue = sb.condense()
      }
      root = new TextFragmentValue(textValue, getBaseURI)
      root.asInstanceOf[TextFragmentValue].setConfiguration(controller.getConfiguration)
    } else {
      try {
        val c2 = context.newMinorContext()
        val builder = controller.makeBuilder()
        builder.setBaseURI(getBaseURI)
        val pipe = controller.makePipelineConfiguration()
        builder.setPipelineConfiguration(pipe)
        c2.changeOutputDestination(builder, false)
        val out = c2.getReceiver
        out.open()
        out.startDocument()
        content.process(c2)
        out.endDocument()
        out.close()
        root = builder.getCurrentRoot.asInstanceOf[DocumentInfo]
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(getSourceLocator)
          throw e
        }
      }
    }
    root
  }
}

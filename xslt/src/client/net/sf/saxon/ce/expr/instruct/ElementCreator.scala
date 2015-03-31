// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.event._
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instruction that creates an element node. There are two subtypes, FixedElement
 * for use where the name is known statically, and Element where it is computed
 * dynamically. To allow use in both XSLT and XQuery, the class acts both as an
 * Instruction and as an Expression.
 */
abstract class ElementCreator extends ParentNodeConstructor {

  /**
   * The inheritNamespaces flag indicates that the namespace nodes on the element created by this instruction
   * are to be inherited (copied) on the children of this element. That is, if this flag is false, the child
   * elements must carry a namespace undeclaration for all the namespaces on the parent, unless they are
   * redeclared in some way.
   */
  protected var inheritNamespaces: Boolean = true

  /**
   * Get the item type of the value returned by this instruction
   * @return the item type
   */
  def getItemType(): ItemType = NodeKindTest.ELEMENT

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  def computeSpecialProperties(): Int = {
    super.computeSpecialProperties() | StaticProperty.SINGLE_DOCUMENT_NODESET
  }

  /**
   * Determine (at run-time) the name code of the element being constructed
   *
   * @param context the XPath dynamic evaluation context
   * @param copiedNode
   * @return the integer name code representing the element name
   * @throws XPathException if a failure occurs
   */
  def getNameCode(context: XPathContext, copiedNode: NodeInfo): StructuredQName

  /**
   * Get the base URI for the element being constructed
   * @param context the XPath dynamic evaluation context
   * @param copiedNode the node being copied (for xsl:copy), otherwise null
   * @return the base URI of the constructed element
   */
  protected def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String

  /**
   * Callback to output namespace nodes for the new element. This method is responsible
   * for ensuring that a namespace node is always generated for the namespace of the element
   * name itself.
   *
   * @param context The execution context
   * @param receiver the Receiver where the namespace nodes are to be written
   * @param nameCode the name code of the element being created
   * @param copiedNode the node being copied (for xsl:copy) or null otherwise
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  protected def outputNamespaceNodes(context: XPathContext, 
      receiver: Receiver, 
      nameCode: StructuredQName, 
      copiedNode: NodeInfo): Unit

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered. For instructions this is the process() method.
   */
  def getImplementationMethod(): Int = {
    Expression.PROCESS_METHOD | Expression.EVALUATE_METHOD
  }

  /**
   * Evaluate the instruction to produce a new element node. This method is typically used when there is
   * a parent element or document in a result tree, to which the new element is added.
   * @param context XPath dynamic evaluation context
   * @return null (this instruction never returns a tail call)
   * @throws XPathException
   */
  def processLeavingTail(context: XPathContext): TailCall = processLeavingTail(context, null)

  /**
   * Evaluate the instruction to produce a new element node. This method is typically used when there is
   * a parent element or document in a result tree, to which the new element is added.
   * @param context XPath dynamic evaluation context
   * @param copiedNode null except in the case of xsl:copy, when it is the node being copied
   * @return null (this instruction never returns a tail call)
   * @throws XPathException
   */
  protected def processLeavingTail(context: XPathContext, copiedNode: NodeInfo): TailCall = {
    try {
      val nameCode = getNameCode(context, copiedNode)
      val out = context.getReceiver
      if (out.getSystemId == null) {
        out.setSystemId(getNewBaseURI(context, copiedNode))
      }
      val properties = if (inheritNamespaces) 0 else ReceiverOptions.DISINHERIT_NAMESPACES
      out.startElement(nameCode, properties)
      outputNamespaceNodes(context, out, nameCode, copiedNode)
      content.process(context)
      out.endElement()
      null
    } catch {
      case e: XPathException ⇒ {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
  }

  /**
   * Evaluate the constructor, returning the constructed element node. If lazy construction
   * mode is in effect, then an UnconstructedParent object is returned instead.
   */
  def evaluateItem(context: XPathContext): Item = constructElement(context, null)

  /**
   * Construct the element node as a free-standing (parentless) node in a tiny tree
   * @param context XPath dynamic evaluation context
   * @return the constructed element node
   * @throws XPathException
   */
  private def constructElement(context: XPathContext, copiedNode: NodeInfo): NodeInfo = {
    try {
      val controller = context.getController
      val c2 = context.newMinorContext()
      val seq = controller.allocateSequenceOutputter(1)
      val pipe = controller.makePipelineConfiguration()
      seq.setPipelineConfiguration(pipe)
      val nameCode = getNameCode(c2, copiedNode)
      c2.setTemporaryReceiver(seq)
      if (seq.getSystemId == null) {
        seq.setSystemId(getNewBaseURI(c2, copiedNode))
      }
      seq.open()
      val properties = if (inheritNamespaces) 0 else ReceiverOptions.DISINHERIT_NAMESPACES
      seq.startElement(nameCode, properties)
      outputNamespaceNodes(c2, seq, nameCode, null)
      content.process(c2)
      seq.endElement()
      seq.close()
      val result = seq.getFirstItem.asInstanceOf[NodeInfo]
      seq.reset()
      result
    } catch {
      case err: XPathException ⇒ {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
  }
}

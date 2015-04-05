// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.event.NoOpenStartTagException
import client.net.sf.saxon.ce.event.SequenceReceiver
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.PromotionOffer
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.CopyOptions
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:copy-of element in the stylesheet.
 */
class CopyOf(var select: Expression, var copyNamespaces: Boolean) extends Instruction {

  private var staticBaseUri: String = _

  adoptChildExpression(select)

  /**
   * Set the static base URI of the xsl:copy-of instruction
   * @param base the static base URI
   */
  def setStaticBaseUri(base: String): Unit = {
    staticBaseUri = base
  }

  /**
   * Determine whether this instruction creates new nodes.
   * The result depends on the type of the select expression.
   */
  def createsNewNodes(): Boolean = {
    !select.getItemType.isInstanceOf[AtomicType]
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  def getImplementationMethod: Int = PROCESS_METHOD

  def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    this
  }

  def getItemType: ItemType = select.getItemType

  def getCardinality: Int = select.getCardinality

  def getDependencies: Int = select.getDependencies

  protected def promoteInst(offer: PromotionOffer): Unit = {
    select = doPromotion(select, offer)
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.typeCheck(select, contextItemType)
    adoptChildExpression(select)
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.optimize(select, contextItemType)
    adoptChildExpression(select)
    if (select.getItemType.isInstanceOf[AtomicType]) {
      return select
    }
    this
  }

  def iterateSubExpressions(): Iterator[Expression] = nonNullChildren(select)

  /**
   * Process this xsl:copy-of instruction
   *
   * @param context the dynamic context for the transformation
   * @return null - this implementation of the method never returns a TailCall
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    val out = context.getReceiver
    val copyBaseURI = out.getSystemId == null
    var copyOptions = CopyOptions.TYPE_ANNOTATIONS
    if (copyNamespaces) {
      copyOptions |= CopyOptions.ALL_NAMESPACES
    }
    val iter = select.iterate(context)
    while (true) {
      val item = iter.next()
      if (item == null) {
        //break
      }
      if (item.isInstanceOf[NodeInfo]) {
        val source = item.asInstanceOf[NodeInfo]
        val kind = source.getNodeKind
        kind match {
          case Type.ELEMENT ⇒
            if (copyBaseURI) {
              out.setSystemId(computeNewBaseUri(source))
            }
            source.copy(out, copyOptions)
            //break
          case Type.ATTRIBUTE ⇒ try {
            context.getReceiver.attribute(source.getNodeName, source.getStringValue)
          } catch {
            case err: NoOpenStartTagException ⇒ dynamicError(err.getMessage, err.getErrorCodeLocalPart)
          }
          case Type.TEXT ⇒ out.characters(source.getStringValue)
          case Type.PROCESSING_INSTRUCTION ⇒
            if (copyBaseURI) {
              out.setSystemId(source.getBaseURI)
            }
            out.processingInstruction(source.getDisplayName, source.getStringValue)

          case Type.COMMENT ⇒ out.comment(source.getStringValue)
          case Type.NAMESPACE ⇒ try {
            source.copy(out, 0)
          } catch {
            case err: NoOpenStartTagException ⇒ dynamicError(err.getMessage, err.getErrorCodeLocalPart)
          }
          case Type.DOCUMENT ⇒
            out.setPipelineConfiguration(out.getPipelineConfiguration)
            if (copyBaseURI) {
              out.setSystemId(source.getBaseURI)
            }
            source.copy(out, copyOptions)
            //break
          case _ ⇒ throw new IllegalArgumentException("Unknown node kind " + source.getNodeKind)
        }
      } else {
        out.append(item, NodeInfo.ALL_NAMESPACES)
      }
    }
    null
  }

  private def computeNewBaseUri(source: NodeInfo): String = {
    var newBaseUri: String = null
    val xmlBase = Navigator.getAttributeValue(source, NamespaceConstant.XML, "base")
    if (xmlBase != null) {
      try {
        val xmlBaseUri = new URI(xmlBase, true)
        if (xmlBaseUri.isAbsolute) {
          newBaseUri = xmlBase
        } else if (staticBaseUri != null) {
          val sbu = new URI(staticBaseUri)
          val abs = sbu.resolve(xmlBaseUri.toString)
          newBaseUri = abs.toString
        } else {
          newBaseUri = source.getBaseURI
        }
      } catch {
        case err: URI.URISyntaxException ⇒ newBaseUri = source.getBaseURI
      }
    } else {
      newBaseUri = source.getBaseURI
    }
    newBaseUri
  }
}

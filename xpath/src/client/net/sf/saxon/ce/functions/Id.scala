// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, Type}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.{DocumentOrderIterator, LocalOrderComparer}
import client.net.sf.saxon.ce.functions.Id._
import client.net.sf.saxon.ce.om.{DocumentInfo, Item, NodeInfo, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.ArrayList
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.{ListIterator, SingletonIterator}
import client.net.sf.saxon.ce.value.{AtomicValue, StringValue, Whitespace}

object Id {

  private class IdMappingFunction extends MappingFunction {

    var document: DocumentInfo = _

    /**
     * Evaluate the function for a single string value
     * (implements the MappingFunction interface)
     */
    def map(item: Item): SequenceIterator = {
      val idrefs = Whitespace.trim(item.getStringValue)
      if (Whitespace.containsWhitespace(idrefs)) {
        val tokens = Whitespace.tokenize(idrefs)
        val refs = new ArrayList[StringValue](tokens.size)
        for (s <- tokens) {
          refs.add(StringValue.makeStringValue(s))
        }
        val submap = new IdMappingFunction()
        submap.document = document
        new MappingIterator(new ListIterator(refs), submap)
      } else {
        SingletonIterator.makeIterator(document.selectID(idrefs))
      }
    }
  }
}

/**
 * The XPath id() or element-with-id() function (synonymous for a non-schema-aware processor)
 * XPath 2.0 version: accepts any sequence as the first parameter; each item in the sequence
 * is taken as an IDREFS value, that is, a space-separated list of ID values.
 * Also accepts an optional second argument to identify the target document, this
 * defaults to the context node.
 */
class Id extends SystemFunction {

  def newInstance(): Id = new Id()

  /**
   * Simplify: add a second implicit argument, the context document
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    val id = super.simplify(visitor).asInstanceOf[Id]
    if (argument.length == 1) {
      id.addContextDocumentArgument(1, getFunctionName.getLocalName)
    }
    id
  }

  /**
   * Type-check the expression. This also calls preEvaluate() to evaluate the function
   * if all the arguments are constant; functions that do not require this behavior
   * can override the preEvaluate method.
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (argument(1).isInstanceOf[RootExpression] && contextItemType != null && 
      contextItemType.isInstanceOf[AtomicType]) {
      typeError(getFunctionName.getLocalName + 
        "() function called when the context item is not a node", "XPTY0004")
    }
    super.typeCheck(visitor, contextItemType)
  }

  /**
   * Static analysis: prevent sorting of the argument
   */
  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    super.checkArguments(visitor)
    argument(0) = ExpressionTool.unsorted(visitor.getConfiguration, argument(0), false)
  }

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * @param visitor an expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    var prop = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.NON_CREATIVE
    if ((getNumberOfArguments == 1) || 
      (argument(1).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  /**
   * Evaluate the function to return an iteration of selected nodes.
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    var arg1: NodeInfo = null
    try {
      arg1 = argument(1).evaluateItem(context).asInstanceOf[NodeInfo]
    } catch {
      case e: XPathException => if (context.getContextItem.isInstanceOf[AtomicValue]) {
        dynamicError("For the " + getFunctionName.getLocalName + "() function, the context item is not a node", 
          "XPTY0004")
        return null
      } else {
        throw e
      }
    }
    arg1 = arg1.getRoot
    if (arg1.getNodeKind != Type.DOCUMENT) {
      dynamicError("In the " + getFunctionName.getLocalName + "() function," + 
        " the tree being searched must be one whose root is a document node", "FODC0001")
      return null
    }
    val doc = arg1.asInstanceOf[DocumentInfo]
    val idrefs = argument(0).iterate(context)
    val map = new IdMappingFunction()
    map.document = doc
    val result = new MappingIterator(idrefs, map)
    new DocumentOrderIterator(result, LocalOrderComparer.getInstance)
  }
}

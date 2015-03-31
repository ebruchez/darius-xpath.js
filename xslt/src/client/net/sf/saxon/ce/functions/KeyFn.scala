// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.DocumentOrderIterator
import client.net.sf.saxon.ce.expr.sort.LocalOrderComparer
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.style.ExpressionContext
import client.net.sf.saxon.ce.trans.KeyDefinitionSet
import client.net.sf.saxon.ce.trans.KeyManager
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.Cardinality
import KeyFn._

import scala.collection.JavaConversions._

object KeyFn {

  /**
   * Mapping class to filter nodes that have the origin node as an ancestor-or-self
   */
  private class SubtreeFilter extends ItemMappingFunction {

    var origin: NodeInfo = _

    def mapItem(item: Item): Item = {
      if (Navigator.isAncestorOrSelf(origin, item.asInstanceOf[NodeInfo])) {
        item
      } else {
        null
      }
    }
  }
}

class KeyFn extends SystemFunction {

  def newInstance(): KeyFn = new KeyFn()

  private var nsContext: NamespaceResolver = null

  private var staticKeySet: KeyDefinitionSet = null

  @transient private var checked: Boolean = false

  @transient private var internal: Boolean = false

  /**
   * Type-check the expression. This also calls preEvaluate() to evaluate the function
   * if all the arguments are constant; functions that do not require this behavior
   * can override the preEvaluate method.
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    try {
      super.typeCheck(visitor, contextItemType)
    } catch {
      case err: XPathException => {
        if ("XPDY0002" == err.getErrorCodeLocalPart) {
          dynamicError("Cannot call the key() function when there is no context node", "XTDE1270")
        }
        throw err
      }
    }
  }

  /**
   * Simplify: add a third implicit argument, the context document
   * @param visitor the expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    if (!internal && 
      !visitor.getStaticContext.isInstanceOf[ExpressionContext]) {
      throw new XPathException("The key() function is available only in XPath expressions within an XSLT stylesheet")
    }
    val f = super.simplify(visitor).asInstanceOf[KeyFn]
    if (argument.length == 2) {
      f.addContextDocumentArgument(2, "key")
    }
    f
  }

  def checkArguments(visitor: ExpressionVisitor): Unit = {
    if (checked) return
    checked = true
    super.checkArguments(visitor)
    argument(1) = ExpressionTool.unsorted(visitor.getConfiguration, argument(1), false)
    if (argument(0).isInstanceOf[StringLiteral]) {
      var keyName: StructuredQName = null
      try {
        keyName = StructuredQName.fromLexicalQName(argument(0).asInstanceOf[StringLiteral].getStringValue, 
          "", nsContext)
      } catch {
        case e: XPathException => {
          val err = new XPathException("Error in key name " + 
            argument(0).asInstanceOf[StringLiteral].getStringValue + 
            ": " + 
            e.getMessage)
          err.setLocator(getSourceLocator)
          err.setErrorCode("XTDE1260")
          throw err
        }
      }
      staticKeySet = visitor.getExecutable.getKeyManager.getKeyDefinitionSet(keyName)
      if (staticKeySet == null) {
        val err = new XPathException("Key " + 
          argument(0).asInstanceOf[StringLiteral].getStringValue + 
          " has not been defined")
        err.setLocator(getSourceLocator)
        err.setErrorCode("XTDE1260")
        throw err
      }
    } else {
      nsContext = visitor.getStaticContext.getNamespaceResolver
    }
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * a property bit is set, it is true, but if it is unset, the value is unknown.
   */
  def computeSpecialProperties(): Int = {
    var prop = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
      StaticProperty.NON_CREATIVE
    if ((getNumberOfArguments == 2) || 
      (argument(2).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing
   * @param visitor the expression visitor
   */
  def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Enumerate the results of the expression
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val controller = context.getController
    var arg2: Item = null
    try {
      arg2 = argument(2).evaluateItem(context)
    } catch {
      case e: XPathException => {
        val code = e.getErrorCodeLocalPart
        if ("XPDY0002" == code) {
          dynamicError("Cannot call the key() function when there is no context item", "XTDE1270")
          return null
        } else if ("XPDY0050" == code) {
          dynamicError("In the key() function," + 
            " the node supplied in the third argument (or the context node if absent)" + 
            " must be in a tree whose root is a document node", "XTDE1270")
          return null
        } else if ("XPTY0020" == code) {
          dynamicError("Cannot call the key() function when the context item is an atomic value", "XTDE1270")
          return null
        }
        throw e
      }
    }
    val origin = arg2.asInstanceOf[NodeInfo]
    val root = origin.getRoot
    if (root.getNodeKind != Type.DOCUMENT) {
      dynamicError("In the key() function," + 
        " the node supplied in the third argument (or the context node if absent)" + 
        " must be in a tree whose root is a document node", "XTDE1270")
      return null
    }
    val doc = root.asInstanceOf[DocumentInfo]
    val keyManager = controller.getExecutable.getKeyManager
    var selectedKeySet = staticKeySet
    if (selectedKeySet == null) {
      val givenkeyname = argument(0).evaluateItem(context).getStringValue
      var qName: StructuredQName = null
      try {
        qName = StructuredQName.fromLexicalQName(givenkeyname, "", nsContext)
      } catch {
        case err: XPathException => dynamicError("Invalid key name: " + err.getMessage, "XTDE1260")
      }
      selectedKeySet = keyManager.getKeyDefinitionSet(qName)
      if (selectedKeySet == null) {
        dynamicError("Key '" + givenkeyname + "' has not been defined", "XTDE1260")
        return null
      }
    }
    val expression = argument(1)
    var allResults: SequenceIterator = null
    if (Cardinality.allowsMany(expression.getCardinality)) {
      val keyContext = context
      val document = doc
      val keySet = selectedKeySet
      val map = new MappingFunction() {

        def map(item: Item): SequenceIterator = {
          return keyManager.selectByKey(keySet, document, item.asInstanceOf[AtomicValue], keyContext)
        }
      }
      val keys = argument(1).iterate(context)
      val allValues = new MappingIterator(keys, map)
      allResults = new DocumentOrderIterator(allValues, LocalOrderComparer.getInstance)
    } else {
      try {
        val keyValue = argument(1).evaluateItem(context).asInstanceOf[AtomicValue]
        if (keyValue == null) {
          return EmptyIterator.getInstance
        }
        allResults = keyManager.selectByKey(selectedKeySet, doc, keyValue, context)
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(getSourceLocator)
          throw e
        }
      }
    }
    if (origin == doc) {
      return allResults
    }
    val filter = new SubtreeFilter()
    filter.origin = origin
    new ItemMappingIterator(allResults, filter)
  }
}

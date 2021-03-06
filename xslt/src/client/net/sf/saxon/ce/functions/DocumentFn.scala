// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.functions

import org.orbeon.darius.xpath.Controller
import org.orbeon.darius.xpath.dom.HTMLDocumentWrapper
import org.orbeon.darius.xpath.dom.Sanitizer
import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.expr.sort.DocumentOrderIterator
import org.orbeon.darius.xpath.expr.sort.GlobalOrderComparer
import org.orbeon.darius.xpath.om._
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.trans.StripSpaceRules
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.SourceLocator
import org.orbeon.darius.xpath.tree.util.URI
import org.orbeon.darius.xpath.value.Cardinality
import DocumentFn._

import scala.collection.JavaConversions._

object DocumentFn {

  private class DocumentMappingFunction(var context: XPathContext) extends ItemMappingFunction {

    var baseURI: String = _

    var stylesheetURI: String = _

    var locator: SourceLocator = _

    def mapItem(item: Item): Item = {
      var b = baseURI
      if (b == null) {
        b = if (item.isInstanceOf[NodeInfo]) item.asInstanceOf[NodeInfo].getBaseURI else stylesheetURI
      }
      makeDoc(item.getStringValue, b, context, locator)
    }
  }

  /**
   * Supporting routine to load one external document given a URI (href) and a baseURI. This is used
   * in the normal case when a document is loaded at run-time (that is, when a Controller is available)
   * @param href the relative URI
   * @param baseURI the base URI
   * @param c the dynamic XPath context
   * @param locator used to identify the location of the instruction in event of error
   * @return the root of the constructed document, or the selected element within the document
   * if a fragment identifier was supplied
   */
  def makeDoc(href: String, 
      baseURI: String, 
      c: XPathContext, 
      locator: SourceLocator): NodeInfo = {
    val config = c.getConfiguration
    val hash = href.indexOf('#')
    var fragmentId: String = null
    if (hash >= 0) {
      if (hash == href.length - 1) {
        href = href.substring(0, hash)
      } else {
        fragmentId = href.substring(hash + 1)
        href = href.substring(0, hash)
        if (!NameChecker.isValidNCName(fragmentId)) {
          fragmentId = null
        }
      }
    }
    val controller = c.getController
    val documentKey = computeDocumentKey(href, baseURI)
    var doc = config.getGlobalDocumentPool.find(documentKey)
    if (doc != null) {
      return doc
    }
    val pool = controller.getDocumentPool
    doc = pool.find(documentKey)
    if (doc != null) {
      return getFragment(doc, fragmentId, c)
    }
    if (!controller.checkUniqueOutputDestination(documentKey)) {
      pool.markUnavailable(documentKey)
      throw new XPathException("Cannot read a document that was written during the same transformation: " + 
        documentKey, "XTRE1500")
    }
    try {
      if (pool.isMarkedUnavailable(documentKey)) {
        throw new XPathException("Document has been marked not available: " + documentKey, "FODC0002")
      }
      val newdoc = config.buildDocument(documentKey.toString)
      if (newdoc.isInstanceOf[HTMLDocumentWrapper]) {
        val rules = c.getController.getExecutable.getStripperRules
        if (rules.isStripping) {
          new Sanitizer(rules).sanitize(newdoc.asInstanceOf[HTMLDocumentWrapper])
        }
      }
      controller.registerDocument(newdoc, documentKey)
      controller.addUnavailableOutputDestination(documentKey)
      getFragment(newdoc, fragmentId, c)
    } catch {
      case err: XPathException ⇒
        pool.markUnavailable(documentKey)
        null
    }
  }

  /**
   * Compute a document key (an absolute URI that can be used to see if a document is already loaded)
   * @param href the relative URI
   * @param baseURI the base URI
   * @return a unique key for the document
   */
  def computeDocumentKey(href: String, baseURI: String): DocumentURI = {
    var documentKey: String = null
    try {
      documentKey = ResolveURI.makeAbsolute(href, baseURI).toString
    } catch {
      case e: URI.URISyntaxException ⇒ documentKey = baseURI + "/../" + href
    }
    new DocumentURI(documentKey)
  }

  /**
   * Resolve the fragment identifier within a URI Reference.
   * Only "bare names" XPointers are recognized, that is, a fragment identifier
   * that matches an ID attribute value within the target document.
   * @param doc the document node
   * @param fragmentId the fragment identifier (an ID value within the document). This will already
   *
   * @param context the XPath dynamic context
   * @return the element within the supplied document that matches the
   * given id value; or null if no such element is found.
   */
  private def getFragment(doc: DocumentInfo, fragmentId: String, context: XPathContext): NodeInfo = {
    if (fragmentId == null) {
      return doc
    }
    doc.selectID(fragmentId)
  }
}

/**
 * Implements the XSLT document() function
 */
class DocumentFn extends SystemFunction {

  def newInstance(): DocumentFn = new DocumentFn()

  private var expressionBaseURI: String = null

  /**
   * Method called during static type checking
   */
  def checkArguments(visitor: ExpressionVisitor): Unit = {
    if (expressionBaseURI == null) {
      super.checkArguments(visitor)
      expressionBaseURI = visitor.getStaticContext.getBaseURI
      argument(0) = ExpressionTool.unsorted(visitor.getConfiguration, argument(0), retainAllNodes = false)
    }
  }

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = {
    val expression = argument(0)
    if (Cardinality.allowsMany(expression.getCardinality)) {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }
  }

  /**
   * Get the base URI from the static context
   * @return the base URI
   */
  def getStaticBaseURI(): String = expressionBaseURI

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  def computeSpecialProperties(): Int = {
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET | 
      StaticProperty.NON_CREATIVE
  }

  /**
   * preEvaluate:
   * @param visitor an expression visitor
   */
  def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * iterate() handles evaluation of the function:
   * it returns a sequence of Document nodes
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val numArgs = argument.length
    val hrefSequence = argument(0).iterate(context)
    var baseURI: String = null
    if (numArgs == 2) {
      val base = argument(1).evaluateItem(context).asInstanceOf[NodeInfo]
      baseURI = base.getBaseURI
    }
    val map = new DocumentMappingFunction(context)
    map.baseURI = baseURI
    map.stylesheetURI = expressionBaseURI
    map.locator = getSourceLocator
    val iter = new ItemMappingIterator(hrefSequence, map)
    val expression = argument(0)
    if (Cardinality.allowsMany(expression.getCardinality)) {
      new DocumentOrderIterator(iter, GlobalOrderComparer.getInstance)
    } else {
      iter
    }
  }
}

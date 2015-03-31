// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import java.net.URI

import client.net.sf.saxon.ce.`type`.{AtomicType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.SortKeyDefinition._
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.orbeon.HashMap
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{StringValue, Whitespace}

import scala.beans.BeanProperty

object SortKeyDefinition {

  private val defaultOrder: StringLiteral = new StringLiteral("ascending")

  private val defaultCaseOrder: StringLiteral = new StringLiteral("#default")

  private val defaultLanguage: StringLiteral = new StringLiteral(StringValue.EMPTY_STRING)

  val ORDER = 0
  val DATA_TYPE = 1
  val CASE_ORDER = 2
  val LANG = 3
  val COLLATION = 4
  val STABLE = 5
  val N = 6
}

class SortKeyDefinition {

  protected var sortKey: Expression = _

  protected var sortProperties: Array[Expression] = new Array[Expression](N)

  protected var collation: StringCollator = _

  protected var baseURI: String = _

  protected var backwardsCompatible: Boolean = false

  @BeanProperty
  var finalComparator: AtomicComparer = null

  sortProperties(ORDER) = defaultOrder

  sortProperties(CASE_ORDER) = defaultCaseOrder

  sortProperties(LANG) = defaultLanguage

  /**
   * Set the expression used as the sort key
   * @param exp the sort key select expression
   */
  def setSortKey(exp: Expression): Unit = {
    sortKey = exp
  }

  /**
   * Get the expression used as the sort key
   * @return the sort key select expression
   */
  def getSortKey(): Expression = sortKey

  def setSortProperty(property: Int, value: Expression): Unit = {
    sortProperties(property) = value
  }

  def getSortProperty(property: Int): Expression = sortProperties(property)

  /**
   * Set the collation to be used
   * @param collation A StringCollator, which encapsulates both the collation URI and the collating function
   */
  def setCollation(collation: StringCollator): Unit = {
    this.collation = collation
  }

  /**
   * Get the collation to be used
   * @return A StringCollator, which encapsulates both the collation URI and the collating function
   */
  def getCollation(): StringCollator = collation

  /**
   * Set the base URI of the expression. This is needed to handle the case where a collation URI
   * evaluated at run-time turns out to be a relative URI.
   * @param baseURI the static base URI of the expression
   */
  def setBaseURI(baseURI: String): Unit = {
    this.baseURI = baseURI
  }

  /**
   * Get the static base URI of the expression. This is needed to handle the case where a collation URI
   * evaluated at run-time turns out to be a relative URI.
   * @return the static base URI of the expression
   */
  def getBaseURI(): String = baseURI

  /**
   * Set whether this sort key is evaluated in XSLT 1.0 backwards compatibility mode
   * @param compatible true if backwards compatibility mode is selected
   */
  def setBackwardsCompatible(compatible: Boolean): Unit = {
    backwardsCompatible = compatible
  }

  /**
   * Ask whether this sort key is evaluated in XSLT 1.0 backwards compatibility mode
   * @return true if backwards compatibility mode was selected
   */
  def isBackwardsCompatible(): Boolean = backwardsCompatible

  /**
   * Ask whether the sort key definition is fixed, that is, whether all the information needed
   * to create a Comparator is known statically
   * @return true if all information needed to create a Comparator is known statically
   */
  def isFixed(): Boolean = {
    (0 until N).find(x => sortProperties(x) != null && !sortProperties(x).isInstanceOf[Literal])
      .map(_ => false)
      .getOrElse(true)
  }

  /**
   * Simplify this sort key definition
   * @param visitor the expression visitor
   * @return the simplified sort key definition
   * @throws XPathException if any failure occurs
   */
  def simplify(visitor: ExpressionVisitor): SortKeyDefinition = {
    sortKey = visitor.simplify(sortKey)
    for (i <- 0 until N) {
      sortProperties(i) = visitor.simplify(sortProperties(i))
    }
    this
  }

  /**
   * Type-check this sort key definition (all properties other than the sort key
   * select expression, which has a different dynamic context)
   * @param visitor the expression visitor
   * @param contextItemType the type of the context item
   * @throws XPathException if any failure occurs
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Unit = {
    for (i <- 0 until N) {
      sortProperties(i) = visitor.typeCheck(sortProperties(i), contextItemType)
    }
    val language = sortProperties(LANG)
    if (language.isInstanceOf[StringLiteral] && 
      language.asInstanceOf[StringLiteral].getStringValue
      .length != 
      0) {
      if (!StringValue.isValidLanguageCode(language.asInstanceOf[StringLiteral].getStringValue)) {
        throw new XPathException("The lang attribute of xsl:sort must be a valid language code", "XTDE0030")
      }
    }
  }

  /**
   * Allocate an AtomicComparer to perform the comparisons described by this sort key component. This method
   * is called at run-time. The AtomicComparer takes into account not only the collation, but also parameters
   * such as order=descending and handling of empty sequence and NaN (the result of the compare()
   * method of the comparator is +1 if the second item is to sort after the first item)
   * @param context the dynamic evaluation context
   * @return an AtomicComparer suitable for making the sort comparisons
   */
  def makeComparator(context: XPathContext): AtomicComparer = {
    val orderX = sortProperties(ORDER).evaluateAsString(context).toString
    val config = context.getConfiguration
    val th = TypeHierarchy.getInstance
    var atomicComparer: AtomicComparer = null
    var stringCollator: StringCollator = null
    if (collation != null) {
      stringCollator = collation
    } else if (sortProperties(COLLATION) != null) {
      val cname = sortProperties(COLLATION).evaluateAsString(context)
        .toString
      var collationURI: URI = null
      collationURI = new URI(cname)//ORBEON true
      if (!collationURI.isAbsolute) {
        if (baseURI == null) {
          throw new XPathException("Collation URI is relative, and base URI is unknown")
        } else {
          val base = new URI(baseURI)
          collationURI = base.resolve(collationURI.toString)
        }
      }
      stringCollator = context.getConfiguration.getNamedCollation(collationURI.toString)
      if (stringCollator == null) {
        throw new XPathException("Unknown collation " + collationURI.toString, "XTDE1035")
      }
    } else {
      val caseOrderX = sortProperties(CASE_ORDER).evaluateAsString(context)
        .toString
      val languageX = sortProperties(LANG).evaluateAsString(context).toString
      val props = new HashMap[String, String]()
      if (languageX.length != 0 && 
        !sortProperties(LANG).isInstanceOf[StringLiteral]) {
        if (!StringValue.isValidLanguageCode(sortProperties(LANG).asInstanceOf[StringLiteral].getStringValue)) {
          throw new XPathException("The lang attribute of xsl:sort must be a valid language code", "XTDE0030")
        }
        props.put("lang", languageX)
      }
      if (caseOrderX != "#default") {
        props.put("case-order", caseOrderX)
      }
      stringCollator = null
    }
    if (sortProperties(DATA_TYPE) == null) {
      atomicComparer = AtomicSortComparer.makeSortComparer(stringCollator, sortKey.getItemType.getAtomizedItemType, 
        context.getImplicitTimezone)
    } else {
      val dataType = sortProperties(DATA_TYPE).evaluateAsString(context)
        .toString
      if (dataType == "text") {
        atomicComparer = AtomicSortComparer.makeSortComparer(stringCollator, AtomicType.STRING, context.getImplicitTimezone)
        atomicComparer = new TextComparer(atomicComparer)
      } else if (dataType == "number") {
        atomicComparer = NumericComparer.getInstance
      } else {
        val err = new XPathException("data-type on xsl:sort must be 'text' or 'number'")
        err.setErrorCode("XTDE0030")
        throw err
      }
    }
    if (sortProperties(STABLE) != null) {
      val stableVal = sortProperties(STABLE).evaluateItem(context).asInstanceOf[StringValue]
      val s = Whitespace.trim(stableVal.getStringValue)
      if (s == "yes" || s == "no") {
      } else {
        val err = new XPathException("Value of 'stable' on xsl:sort must be 'yes' or 'no'")
        err.setErrorCode("XTDE0030")
        throw err
      }
    }
    if (orderX == "ascending") {
      atomicComparer
    } else if (orderX == "descending") {
      new DescendingComparer(atomicComparer)
    } else {
      val err1 = new XPathException("order must be 'ascending' or 'descending'")
      err1.setErrorCode("XTDE0030")
      throw err1
    }
  }
}

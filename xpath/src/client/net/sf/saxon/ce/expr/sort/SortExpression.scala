// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.{ArrayList, Iterator}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.value.Cardinality

/**
 * Expression equivalent to the imaginary syntax
 * expr sortby (sort-key)+
 */
class SortExpression(var select: Expression, var sortKeyDefinitions: Array[SortKeyDefinition])
    extends Expression with SortKeyEvaluator {

  private var comparators: Array[AtomicComparer] = null

  val children = iterateSubExpressions()

  while (children.hasNext) {
    val exp = children.next()
    adoptChildExpression(exp)
  }

  /**
   * Get the expression defining the sequence being sorted
   * @return the expression whose result is to be sorted
   */
  def getBaseExpression(): Expression = select

  /**
   * Get the immediate sub-expressions of this expression. Default implementation
   * returns a zero-length array, appropriate for an expression that has no
   * sub-expressions.
   *
   * @return an iterator containing the sub-expressions of this expression
   */
  override def iterateSubExpressions(): Iterator[Expression] = iterateSubExpressions(true)

  private def iterateSubExpressions(includeSortKey: Boolean): Iterator[Expression] = {
    val list = new ArrayList[Expression](8)
    list.add(select)
    for (skd <- sortKeyDefinitions) {
      if (includeSortKey) {
        list.add(skd.getSortKey)
      }
      for (i <- 0 until SortKeyDefinition.N) {
        val e = skd.getSortProperty(i)
        if (e != null) {
          list.add(e)
        }
      }
    }
    list.iterator()
  }

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  override def hasLoopingSubexpression(child: Expression): Boolean = isSortKey(child)

  /**
   * Simplify an expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val select2 = visitor.typeCheck(select, contextItemType)
    if (select2 != select) {
      adoptChildExpression(select2)
      select = select2
    }
    val sortedItemType = select.getItemType
    var allKeysFixed = true
    for (sortKeyDefinition <- sortKeyDefinitions if ! sortKeyDefinition.isFixed) {
      allKeysFixed = false
      //break
    }
    if (allKeysFixed) {
      comparators = new Array[AtomicComparer](sortKeyDefinitions.length)
    }
    for (i <- 0 until sortKeyDefinitions.length) {
      var sortKey = sortKeyDefinitions(i).getSortKey
      sortKey = visitor.typeCheck(sortKey, sortedItemType)
      if (visitor.getStaticContext.isInBackwardsCompatibleMode) {
        sortKey = new FirstItemExpression(sortKey)
      } else {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:sort/select", 0)
        role.setErrorCode("XTTE1020")
        sortKey = CardinalityChecker.makeCardinalityChecker(sortKey, StaticProperty.ALLOWS_ZERO_OR_ONE, 
          role)
      }
      sortKeyDefinitions(i).setSortKey(sortKey)
      sortKeyDefinitions(i).typeCheck(visitor, contextItemType)
      if (sortKeyDefinitions(i).isFixed) {
        val comp = sortKeyDefinitions(i).makeComparator(new EarlyEvaluationContext(visitor.getConfiguration))
        sortKeyDefinitions(i).setFinalComparator(comp)
        if (allKeysFixed) {
          comparators(i) = comp
        }
      }
    }
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[client.net.sf.saxon.ce.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val select2 = visitor.optimize(select, contextItemType)
    if (select2 != select) {
      adoptChildExpression(select2)
      select = select2
    }
    val sortedItemType = select.getItemType
    for (sortKeyDefinition <- sortKeyDefinitions) {
      var sortKey = sortKeyDefinition.getSortKey
      sortKey = visitor.optimize(sortKey, sortedItemType)
      sortKeyDefinition.setSortKey(sortKey)
    }
    if (Cardinality.allowsMany(select.getCardinality)) {
      this
    } else {
      select
    }
  }

  /**
   * Offer promotion for this subexpression. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *              expressions that don't depend on the context to an outer level in
   *              the containing expression
   * @param parent
   * @return if the offer is not accepted, return this expression unchanged.
   *         Otherwise return the result of rewriting the expression to promote
   *         this subexpression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any error is detected
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      select = doPromotion(select, offer)
      for (skd <- sortKeyDefinitions) {
        val sk2 = skd.getSortKey.promote(offer, parent)
        skd.setSortKey(sk2)
        for (i <- 0 until SortKeyDefinition.N) {
          val e = skd.getSortProperty(i)
          if (e != null) {
            skd.setSortProperty(i, e.promote(offer, parent))
          }
        }
      }
      this
    }
  }

  /**
   * Test whether a given expression is one of the sort keys
   * @param child the given expression
   * @return true if the given expression is one of the sort keys
   */
  def isSortKey(child: Expression): Boolean = {
    for (skd <- sortKeyDefinitions) {
      val exp = skd.getSortKey
      if (exp == child) {
        return true
      }
    }
    false
  }

  /**
   * Determine the static cardinality
   */
  def computeCardinality(): Int = select.getCardinality

  /**
   * Determine the data type of the items returned by the expression, if possible
   *
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   *         or Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = select.getItemType

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-significant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties(): Int = {
    var props = 0
    if ((select.getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 
      0) {
      props |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    if ((select.getSpecialProperties & StaticProperty.SINGLE_DOCUMENT_NODESET) != 
      0) {
      props |= StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    if ((select.getSpecialProperties & StaticProperty.NON_CREATIVE) != 
      0) {
      props |= StaticProperty.NON_CREATIVE
    }
    props
  }

  /**
   * Enumerate the results of the expression
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    val iter = select.iterate(context)
    if (iter.isInstanceOf[EmptyIterator]) {
      return iter
    }
    val xpc = context.newMinorContext()
    var comps = comparators
    if (comparators == null) {
      comps = new Array[AtomicComparer](sortKeyDefinitions.length)
      for (s <- 0 until sortKeyDefinitions.length) {
        var comp = sortKeyDefinitions(s).getFinalComparator
        if (comp == null) {
          comp = sortKeyDefinitions(s).makeComparator(xpc)
        }
        comps(s) = comp
      }
    }
    new SortedIterator(xpc, iter, this, comps)
  }

  /**
   * Callback for evaluating the sort keys
   */
  def evaluateSortKey(n: Int, c: XPathContext): Item = {
    sortKeyDefinitions(n).getSortKey.evaluateItem(c)
  }
}

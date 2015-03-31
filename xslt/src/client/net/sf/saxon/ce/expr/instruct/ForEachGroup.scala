// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.LogController
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort._
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.lib.TraceListener
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.PatternSponsor
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.FocusIterator
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.StringValue
import com.google.gwt.logging.client.LogConfiguration
import java.util.ArrayList
import java.util.Iterator
import ForEachGroup._
//remove if not needed
import scala.collection.JavaConversions._

object ForEachGroup {

  val GROUP_BY = 0

  val GROUP_ADJACENT = 1

  val GROUP_STARTING = 2

  val GROUP_ENDING = 3
}

/**
 * Handler for xsl:for-each-group elements in stylesheet. This is a new instruction
 * defined in XSLT 2.0
 */
class ForEachGroup(var select: Expression, 
    var action: Expression, 
    var algorithm: Int, 
    var key: Expression, 
    var collationNameExpression: Expression, 
    var baseURI: String, 
    var sortKeys: Array[SortKeyDefinition]) extends Instruction with ContextMappingFunction with SortKeyEvaluator {

  @transient private var sortComparators: Array[AtomicComparer] = null

  adoptChildExpressions()

  private def adoptChildExpressions(): Unit = {
    adoptChildExpression(select)
    adoptChildExpression(action)
    adoptChildExpression(key)
    adoptChildExpression(collationNameExpression)
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @return the simplified expression
   * @throws XPathException if an error is discovered during expression
   *                        rewriting
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    action = visitor.simplify(action)
    key = visitor.simplify(key)
    collationNameExpression = visitor.simplify(collationNameExpression)
    adoptChildExpressions()
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.typeCheck(select, contextItemType)
    val selectedItemType = select.getItemType
    action = visitor.typeCheck(action, selectedItemType)
    key = visitor.typeCheck(key, selectedItemType)
    collationNameExpression = visitor.typeCheck(collationNameExpression, contextItemType)
    adoptChildExpressions()
    if (sortKeys != null) {
      var allFixed = true
      for (skd ← sortKeys) {
        var sortKey = skd.getSortKey
        sortKey = visitor.typeCheck(sortKey, selectedItemType)
        if (visitor.getStaticContext.isInBackwardsCompatibleMode) {
          sortKey = new FirstItemExpression(sortKey)
        } else {
          val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:sort/select", 0)
          role.setErrorCode("XTTE1020")
          sortKey = CardinalityChecker.makeCardinalityChecker(sortKey, StaticProperty.ALLOWS_ZERO_OR_ONE, 
            role)
        }
        skd.setSortKey(sortKey)
        if (skd.isFixed) {
          val comp = skd.makeComparator(new EarlyEvaluationContext(visitor.getConfiguration))
          skd.setFinalComparator(comp)
        } else {
          allFixed = false
        }
      }
      if (allFixed) {
        sortComparators = Array.ofDim[AtomicComparer](sortKeys.length)
        for (i ← 0 until sortKeys.length) {
          sortComparators(i) = sortKeys(i).getFinalComparator
        }
      }
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    select = visitor.optimize(select, contextItemType)
    action = action.optimize(visitor, select.getItemType)
    key = key.optimize(visitor, select.getItemType)
    adoptChildExpression(select)
    adoptChildExpression(action)
    adoptChildExpression(key)
    if (Literal.isEmptySequence(select)) {
      return select
    }
    if (Literal.isEmptySequence(action)) {
      return action
    }
    val selectedItemType = select.getItemType
    if (sortKeys != null) {
      for (skd ← sortKeys) {
        var sortKey = skd.getSortKey
        sortKey = visitor.optimize(sortKey, selectedItemType)
        skd.setSortKey(sortKey)
      }
    }
    this
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   *
   * @return the static item type of the instruction
   */
  def getItemType(): ItemType = action.getItemType

  /**
   * Compute the dependencies of an expression, as the union of the
   * dependencies of its subexpressions. (This is overridden for path expressions
   * and filter expressions, where the dependencies of a subexpression are not all
   * propogated). This method should be called only once, to compute the dependencies;
   * after that, getDependencies should be used.
   *
   * @return the depencies, as a bit-mask
   */
  def computeDependencies(): Int = {
    var dependencies = 0
    dependencies |= select.getDependencies
    dependencies |= key.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS
    dependencies |= (action.getDependencies & 
      ~(StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_CURRENT_GROUP))
    if (sortKeys != null) {
      for (skd ← sortKeys) {
        dependencies |= (skd.getSortKey.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS)
        for (i ← 0 until SortKeyDefinition.N) {
          val e = skd.getSortProperty(i)
          if (e != null && !e.isInstanceOf[Literal]) {
            dependencies |= e.getDependencies
          }
        }
      }
    }
    if (collationNameExpression != null) {
      dependencies |= collationNameExpression.getDependencies
    }
    dependencies
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true if the "action" creates new nodes.
   * (Nodes created by the condition can't contribute to the result).
   */
  def createsNewNodes(): Boolean = {
    val props = action.getSpecialProperties
    (props & StaticProperty.NON_CREATIVE) == 0
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   *
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    select = doPromotion(select, offer)
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    val list = new ArrayList[Expression](8)
    list.add(select)
    list.add(action)
    list.add(key)
    if (collationNameExpression != null) {
      list.add(collationNameExpression)
    }
    if (sortKeys != null) {
      for (skd ← sortKeys) {
        list.add(skd.getSortKey)
        for (i ← 0 until SortKeyDefinition.N) {
          val e = skd.getSortProperty(i)
          if (e != null) {
            list.add(e)
          }
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
  def hasLoopingSubexpression(child: Expression): Boolean = child == action || child == key

  def processLeavingTail(context: XPathContext): TailCall = {
    val groupIterator = getGroupIterator(context)
    val c2 = context.newContext()
    val focus = c2.setCurrentIterator(groupIterator)
    c2.setCurrentGroupIterator(groupIterator)
    c2.setCurrentTemplateRule(null)
    if (LogConfiguration.loggingIsEnabled() && LogController.traceIsEnabled()) {
      val listener = LogController.getTraceListener
      while (true) {
        val item = focus.next()
        if (item == null) {
          //break
        }
        listener.startCurrentItem(item)
        action.process(c2)
        listener.endCurrentItem(item)
      }
    } else {
      while (true) {
        val item = focus.next()
        if (item == null) {
          //break
        }
        action.process(c2)
      }
    }
    null
  }

  /**
   * Get (and if necessary, create) the comparator used for comparing grouping key values
   * @param context XPath dynamic context
   * @return a StringCollator suitable for comparing the values of grouping keys
   * @throws XPathException
   */
  private def getCollator(context: XPathContext): StringCollator = {
    if (collationNameExpression != null) {
      val collationValue = collationNameExpression.evaluateItem(context).asInstanceOf[StringValue]
      var cname = collationValue.getStringValue
      var collationURI: URI = null
      try {
        collationURI = new URI(cname, true)
        if (!collationURI.isAbsolute) {
          if (baseURI == null) {
            dynamicError("Cannot resolve relative collation URI '" + cname + "': unknown or invalid base URI", 
              "XTDE1110")
          }
          collationURI = new URI(baseURI).resolve(collationURI.toString)
          cname = collationURI.toString
        }
      } catch {
        case e: URI.URISyntaxException ⇒ dynamicError("Collation name '" + cname + "' is not a valid URI",
          "XTDE1110")
      }
      context.getConfiguration.getNamedCollation(cname)
    } else {
      CodepointCollator.getInstance
    }
  }

  private def getGroupIterator(context: XPathContext): GroupIterator = {
    val population = select.iterate(context)
    var groupIterator: GroupIterator = null
    algorithm match {
      case GROUP_BY ⇒ {
        val c2 = context.newMinorContext()
        c2.setCurrentIterator(population)
        groupIterator = new GroupByIterator(population, key, c2, getCollator(context))
        //break
      }
      case GROUP_ADJACENT ⇒ {
        groupIterator = new GroupAdjacentIterator(population, key, context, getCollator(context))
        //break
      }
      case GROUP_STARTING ⇒ groupIterator = new GroupStartingIterator(population, key.asInstanceOf[PatternSponsor].getPattern,
        context)
      case GROUP_ENDING ⇒ groupIterator = new GroupEndingIterator(population, key.asInstanceOf[PatternSponsor].getPattern,
        context)
      case _ ⇒ throw new AssertionError("Unknown grouping algorithm")
    }
    if (sortKeys != null) {
      var comps = sortComparators
      val xpc = context.newMinorContext()
      if (comps == null) {
        comps = Array.ofDim[AtomicComparer](sortKeys.length)
        for (s ← 0 until sortKeys.length) {
          comps(s) = sortKeys(s).makeComparator(xpc)
        }
      }
      groupIterator = new SortedGroupIterator(xpc, groupIterator, this, comps)
    }
    groupIterator
  }

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation relies on the process() method: it
   * "pushes" the results of the instruction to a sequence in memory, and then
   * iterates over this in-memory sequence.
   * <p/>
   * In principle instructions should implement a pipelined iterate() method that
   * avoids the overhead of intermediate storage.
   *
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *         of the expression
   * @throws XPathException if any dynamic error occurs evaluating the
   *                        expression
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val master = getGroupIterator(context)
    val c2 = context.newContext()
    c2.setCurrentIterator(master)
    c2.setCurrentGroupIterator(master)
    c2.setCurrentTemplateRule(null)
    new ContextMappingIterator(this, c2)
  }

  /**
   * Map one item to a sequence.
   *
   * @param context The processing context. This is supplied only for mapping constructs that
   *                set the context node, position, and size. Otherwise it is null.
   * @return either (a) a SequenceIterator over the sequence of items that the supplied input
   *         item maps to, or (b) an Item if it maps to a single item, or (c) null if it maps to an empty
   *         sequence.
   */
  def map(context: XPathContext): SequenceIterator = action.iterate(context)

  /**
   * Callback for evaluating the sort keys
   */
  def evaluateSortKey(n: Int, c: XPathContext): Item = sortKeys(n).getSortKey.evaluateItem(c)
}

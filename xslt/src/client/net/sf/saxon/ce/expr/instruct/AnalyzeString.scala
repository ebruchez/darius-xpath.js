// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.regex.ARegexIterator
import client.net.sf.saxon.ce.regex.ARegularExpression
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.FocusIterator
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.SequenceType
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:analyze-string element in the stylesheet. New at XSLT 2.0
 */
class AnalyzeString(var select: Expression, 
    var regex: Expression, 
    var flags: Expression, 
    var matching: Expression, 
    var nonMatching: Expression) extends Instruction {

  val kids = iterateSubExpressions()

  while (kids.hasNext) {
    val child = kids.next().asInstanceOf[Expression]
    adoptChildExpression(child)
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is prefered.
   */
  def getImplementationMethod(): Int = {
    Expression.PROCESS_METHOD | Expression.ITERATE_METHOD
  }

  /**
   * Get the expression used to process matching substrings
   *
   * @return the expression used to process matching substrings
   */
  def getMatchingExpression(): Expression = matching

  /**
   * Get the expression used to process non-matching substrings
   *
   * @return the expression used to process non-matching substrings
   */
  def getNonMatchingExpression(): Expression = nonMatching

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression).
   *
   * @param visitor an expression visitor
   * @return the simplified expression
   * @throws XPathException if an error is discovered during expression
   *                        rewriting
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    select = visitor.simplify(select)
    regex = visitor.simplify(regex)
    flags = visitor.simplify(flags)
    matching = visitor.simplify(matching)
    nonMatching = visitor.simplify(nonMatching)
    this
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.typeCheck(select, contextItemType)
    adoptChildExpression(select)
    regex = visitor.typeCheck(regex, contextItemType)
    adoptChildExpression(regex)
    flags = visitor.typeCheck(flags, contextItemType)
    adoptChildExpression(flags)
    if (matching != null) {
      matching = visitor.typeCheck(matching, AtomicType.STRING)
      adoptChildExpression(matching)
    }
    if (nonMatching != null) {
      nonMatching = visitor.typeCheck(nonMatching, AtomicType.STRING)
      adoptChildExpression(nonMatching)
    }
    var role = new RoleLocator(RoleLocator.INSTRUCTION, "analyze-string/select", 0)
    val required = SequenceType.SINGLE_STRING
    select = TypeChecker.staticTypeCheck(select, required, false, role)
    role = new RoleLocator(RoleLocator.INSTRUCTION, "analyze-string/regex", 0)
    regex = TypeChecker.staticTypeCheck(regex, SequenceType.SINGLE_STRING, false, role)
    role = new RoleLocator(RoleLocator.INSTRUCTION, "analyze-string/flags", 0)
    flags = TypeChecker.staticTypeCheck(flags, SequenceType.SINGLE_STRING, false, role)
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    select = visitor.optimize(select, contextItemType)
    adoptChildExpression(select)
    regex = visitor.optimize(regex, contextItemType)
    adoptChildExpression(regex)
    flags = visitor.optimize(flags, contextItemType)
    adoptChildExpression(flags)
    if (matching != null) {
      matching = matching.optimize(visitor, AtomicType.STRING)
      adoptChildExpression(matching)
    }
    if (nonMatching != null) {
      nonMatching = nonMatching.optimize(visitor, AtomicType.STRING)
      adoptChildExpression(nonMatching)
    }
    this
  }

  /**
   * Get the item type of the items returned by evaluating this instruction
   *
   * @return the static item type of the instruction
   */
  def getItemType(): ItemType = {
    if (matching != null) {
      if (nonMatching != null) {
        Type.getCommonSuperType(matching.getItemType, nonMatching.getItemType)
      } else {
        matching.getItemType
      }
    } else {
      if (nonMatching != null) {
        nonMatching.getItemType
      } else {
        EmptySequenceTest.getInstance
      }
    }
  }

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
    dependencies |= regex.getDependencies
    dependencies |= flags.getDependencies
    if (matching != null) {
      dependencies |= (matching.getDependencies & 
        ~(StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_REGEX_GROUP))
    }
    if (nonMatching != null) {
      dependencies |= (nonMatching.getDependencies & 
        ~(StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_REGEX_GROUP))
    }
    dependencies
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   *
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer): Unit = {
    select = doPromotion(select, offer)
    regex = doPromotion(regex, offer)
    flags = doPromotion(flags, offer)
    if (matching != null) {
      matching = doPromotion(matching, offer)
    }
    if (nonMatching != null) {
      nonMatching = doPromotion(nonMatching, offer)
    }
  }

  /**
   * Get all the XPath expressions associated with this instruction
   * (in XSLT terms, the expression present on attributes of the instruction,
   * as distinct from the child instructions in a sequence construction)
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    nonNullChildren(select, regex, flags, matching, nonMatching)
  }

  /**
   * Given an expression that is an immediate child of this expression, test whether
   * the evaluation of the parent expression causes the child expression to be
   * evaluated repeatedly
   *
   * @param child the immediate subexpression
   * @return true if the child expression is evaluated repeatedly
   */
  def hasLoopingSubexpression(child: Expression): Boolean = {
    child == matching || child == nonMatching
  }

  /**
   * ProcessLeavingTail: called to do the real work of this instruction. This method
   * must be implemented in each subclass. The results of the instruction are written
   * to the current Receiver, which can be obtained via the Controller.
   *
   * @param context The dynamic context of the transformation, giving access to the current node,
   *                the current variables, etc.
   * @return null if the instruction has completed execution; or a TailCall indicating
   *         a function call or template call that is delegated to the caller, to be made after the stack has
   *         been unwound so as to save stack space.
   */
  def processLeavingTail(context: XPathContext): TailCall = {
    val iter = getRegexIterator(context)
    val c2 = context.newContext()
    val focus = c2.setCurrentIterator(iter)
    c2.setCurrentRegexIterator(iter)
    while (true) {
      val it = focus.next()
      if (it == null) {
        //break
      }
      if (iter.isMatching) {
        if (matching != null) {
          matching.process(c2)
        }
      } else {
        if (nonMatching != null) {
          nonMatching.process(c2)
        }
      }
    }
    null
  }

  /**
   * Get an iterator over the substrings defined by the regular expression
   *
   * @param context the evaluation context
   * @return an iterator that returns matching and nonmatching substrings
   * @throws XPathException if a dynamic error occurs
   */
  private def getRegexIterator(context: XPathContext): ARegexIterator = {
    val input = select.evaluateAsString(context)
    val flagstr = flags.evaluateAsString(context).toString
    val re = new ARegularExpression(regex.evaluateAsString(context), flagstr, "XP20", null)
    if (re.matches("")) {
      dynamicError("The regular expression must not be one that matches a zero-length string", "XTDE1150")
    }
    re.analyze(input)
  }

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation handles iteration for expressions that
   * return singleton values: for non-singleton expressions, the subclass must
   * provide its own implementation.
   *
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *         of the expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any dynamic error occurs evaluating the
   *          expression
   */
  def iterate(context: XPathContext): SequenceIterator = {
    val iter = getRegexIterator(context)
    val c2 = context.newContext()
    c2.setCurrentIterator(iter)
    c2.setCurrentRegexIterator(iter)
    val fn = new AnalyzeMappingFunction(iter, c2)
    new ContextMappingIterator(fn, c2)
  }

  /**
   * Mapping function that maps the sequence of matching/non-matching strings to the
   * sequence delivered by applying the matching-substring and non-matching-substring
   * expressions respectively to each such string
   */
  private class AnalyzeMappingFunction(var base: ARegexIterator, var c2: XPathContext)
      extends ContextMappingFunction {

    /**
     * Map one item to a sequence.
     *
     * @param context The processing context. Some mapping functions use this because they require
     *                context information. Some mapping functions modify the context by maintaining the context item
     *                and position. In other cases, the context may be null.
     * @return either (a) a SequenceIterator over the sequence of items that the supplied input
     *         item maps to, or (b) an Item if it maps to a single item, or (c) null if it maps to an empty
     *         sequence.
     */
    def map(context: XPathContext): SequenceIterator = {
      if (base.isMatching) {
        if (matching != null) {
          return matching.iterate(c2)
        }
      } else {
        if (nonMatching != null) {
          return nonMatching.iterate(c2)
        }
      }
      EmptyIterator.getInstance
    }
  }
}

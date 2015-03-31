// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.pattern

import java.util.{Collections, Iterator}
import client.net.sf.saxon.ce.`type`.{ItemType, Type}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.DocumentSorter
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.{EmptyIterator, PrependIterator, SingletonIterator}
import client.net.sf.saxon.ce.tree.util.SourceLocator

import scala.beans.BeanProperty

object Pattern {

  /**
   * Static factory method to make a Pattern by parsing a String. <br>
   *
   * @param pattern   The pattern text as a String
   * @param env       An object defining the compile-time context for the expression
   * @param container The stylesheet element containing this pattern
   * @return The pattern object
   * @throws XPathException if the pattern cannot be converted
   */
  def make(pattern: String, env: StaticContext, container: StyleElement): Pattern = {
    val parser = new PatternParser()
    parser.setLanguage(ExpressionParser.XSLT_PATTERN)
    parser.setDefaultContainer(container)
    var pat = parser.parsePattern(pattern, env)
    pat.setSystemId(env.getSystemId)
    pat.setOriginalText(pattern)
    pat.setExecutable(container.getExecutable)
    val visitor = ExpressionVisitor.make(env, container.getExecutable)
    pat = pat.simplify(visitor)
    pat
  }

  /**
   * Static factory method to make a pattern by converting an expression. The supplied
   * expression is the equivalent expression to the pattern, in the sense that it takes
   * the same syntactic form.
   *
   * <p>Note that this method does NOT check all the rules for XSLT patterns; it deliberately allows
   * a (slightly) wider class of expressions to be converted than XSLT allows.</p>
   *
   * <p>The expression root() at the start of the expression has a special meaning: it refers to
   * the root of the subtree in which the pattern must match, which can be supplied at run-time
   * during pattern matching. This is used for patterns that represent streamable path expressions.</p>
   *
   * @param expression the expression to be converted
   * @param config the Saxon configuration
   * @throws XPathException if the expression cannot be converted
   */
  def fromExpression(expression: Expression, config: Configuration): Pattern = {
    var result: Pattern = null
    if (expression.isInstanceOf[DocumentSorter]) {
      expression = expression.asInstanceOf[DocumentSorter].getBaseExpression
    }
    if (expression.isInstanceOf[VennExpression] && 
      expression.asInstanceOf[VennExpression].getOperator == 
      Token.UNION) {
      result = new UnionPattern(fromExpression(expression.asInstanceOf[VennExpression].getOperands()(0), 
        config), fromExpression(expression.asInstanceOf[VennExpression].getOperands()(1), config))
    } else if (expression.isInstanceOf[AxisExpression]) {
      val axis = expression.asInstanceOf[AxisExpression].getAxis
      var test = expression.asInstanceOf[AxisExpression].getNodeTest
      if (test == null) {
        test = AnyNodeTest.getInstance
      }
      if (test.isInstanceOf[AnyNodeTest] && (axis == Axis.CHILD || axis == Axis.DESCENDANT)) {
        test = AnyChildNodeTest.getInstance
      }
      val kind = test.getRequiredNodeKind
      if (axis == Axis.SELF && kind == Type.DOCUMENT) {
        result = new NodeTestPattern(test)
      } else if (axis == Axis.ATTRIBUTE) {
        result = if (kind == Type.NODE) new NodeTestPattern(NodeKindTest.ATTRIBUTE) else if (!Axis.containsNodeKind(axis, 
          kind)) new NodeTestPattern(EmptySequenceTest.getInstance) else new NodeTestPattern(test)
      } else if (axis == Axis.CHILD || axis == Axis.DESCENDANT || axis == Axis.DESCENDANT_OR_SELF) {
        if (kind != Type.NODE && !Axis.containsNodeKind(axis, kind)) {
          test = EmptySequenceTest.getInstance
        }
        result = new NodeTestPattern(test)
      } else {
        throw new XPathException("Only downwards axes are allowed in a pattern", "XTSE0340")
      }
    } else if (expression.isInstanceOf[FilterExpression]) {
      val base = expression.asInstanceOf[FilterExpression].getControllingExpression
      val filter = expression.asInstanceOf[FilterExpression].getFilter
      var basePattern = fromExpression(base, config)
      if (basePattern.isInstanceOf[NodeTestPattern]) {
        val path = new LocationPathPattern()
        path.setNodeTest(basePattern.getNodeTest)
        basePattern = path
      }
      if (!basePattern.isInstanceOf[LocationPathPattern]) {
        throw new XPathException("The filtered expression in a pattern must be a simple step")
      }
      basePattern.asInstanceOf[LocationPathPattern].addFilter(filter)
      result = basePattern
    } else if (expression.isInstanceOf[SlashExpression]) {
      val head = expression.asInstanceOf[SlashExpression].getLeadingSteps
      val tail = expression.asInstanceOf[SlashExpression].getLastStep
      var tailPattern = fromExpression(tail, config)
      if (tailPattern.isInstanceOf[NodeTestPattern]) {
        val path = new LocationPathPattern()
        path.setNodeTest(tailPattern.getNodeTest)
        tailPattern = path
      }
      if (!tailPattern.isInstanceOf[LocationPathPattern]) {
        throw new XPathException("The path in a pattern must contain simple steps: found " + 
          tailPattern.toString)
      }
      if (tailPattern.asInstanceOf[LocationPathPattern].getUpperPattern != 
        null) {
        throw new XPathException("The path in a pattern must contain simple steps")
      }
      val axis = getAxisForPathStep(tail)
      val headPattern = fromExpression(head, config)
      tailPattern.asInstanceOf[LocationPathPattern].setUpperPattern(axis, headPattern)
      result = tailPattern
    } else if (expression.isInstanceOf[RootExpression]) {
      result = new NodeTestPattern(NodeKindTest.DOCUMENT)
//ORBEON unneeded
//    } else if (expression.isInstanceOf[IXSLFunction]) {
//      result = new JSObjectPattern(expression, config)
    } else {
      val `type` = expression.getItemType
      if (((expression.getDependencies & StaticProperty.DEPENDS_ON_NON_DOCUMENT_FOCUS) == 
        0) && 
        (`type`.isInstanceOf[NodeTest] || expression.isInstanceOf[VariableReference])) {
        result = new NodeSetPattern(expression)
      }
    }
    if (result == null) {
      throw new XPathException("Cannot convert the expression {" + expression.toString + 
        "} to a pattern")
    } else {
      result.setOriginalText(expression.toString)
      result
    }
  }

  private def getAxisForPathStep(step: Expression): Byte = {
    if (step.isInstanceOf[AxisExpression]) {
      Axis.inverseAxis(step.asInstanceOf[AxisExpression].getAxis)
    } else if (step.isInstanceOf[FilterExpression]) {
      getAxisForPathStep(step.asInstanceOf[FilterExpression].getControllingExpression)
    } else if (step.isInstanceOf[PathExpression]) {
      getAxisForPathStep(step.asInstanceOf[PathExpression].getFirstStep)
    } else if (step.isInstanceOf[ContextItemExpression]) {
      Axis.SELF
    } else {
      throw new XPathException("The path in a pattern must contain simple steps")
    }
  }
}

/**
 * A Pattern represents the result of parsing an XSLT pattern string. <br>
 * Patterns are created by calling the static method Pattern.make(string). <br>
 * The pattern is used to test a particular node by calling match().
 */
abstract class Pattern extends Container with SourceLocator {

  private var originalText: String = _

  @BeanProperty
  var executable: Executable = _

  @BeanProperty
  var systemId: String = _

  private var variableBinding: Expression = null

  /**
   * Set an expression used to bind the variable that represents the value of the current() function
   * @param exp the expression that binds the variable
   */
  def setVariableBindingExpression(exp: Expression): Unit = {
    variableBinding = exp
  }

  def getVariableBindingExpression(): Expression = variableBinding

  protected def bindCurrent(node: NodeInfo, context: XPathContext): Unit = {
    if (variableBinding != null) {
      var c2 = context
      val ci = context.getContextItem
      if (!(ci.isInstanceOf[NodeInfo] && ci.asInstanceOf[NodeInfo].isSameNodeInfo(node))) {
        c2 = context.newContext()
        c2.setSingletonFocus(node)
      }
      try {
        variableBinding.evaluateItem(c2)
      } catch {
        case e: XPathException ⇒ return
      }
    }
  }

  /**
   * Get the granularity of the container.
   *
   * @return 0 for a temporary container created during parsing; 1 for a container
   *         that operates at the level of an XPath expression; 2 for a container at the level
   *         of a global function or template
   */
  def getContainerGranularity: Int = 1

  /**
   * Set the original text of the pattern for use in diagnostics
   *
   * @param text the original text of the pattern
   */
  def setOriginalText(text: String): Unit = {
    originalText = text
  }

  /**
   * Simplify the pattern by applying any context-independent optimisations.
   * Default implementation does nothing.
   *
   * @param visitor the expression visitor
   * @return the optimised Pattern
   */
  def simplify(visitor: ExpressionVisitor): Pattern = this

  /**
   * Type-check the pattern.
   *
   * @param visitor         the expression visitor
   * @param contextItemType the type of the context item at the point where the pattern
   *                        is defined. Set to null if it is known that the context item is undefined.
   * @return the optimised Pattern
   */
  def analyze(visitor: ExpressionVisitor, contextItemType: ItemType): Pattern = this

  /**
   * Get the dependencies of the pattern. The only possible dependency for a pattern is
   * on local variables. This is analyzed in those patterns where local variables may appear.
   *
   * @return the dependencies, as a bit-significant mask
   */
  def getDependencies(): Int = 0

  /**
   * Iterate over the subexpressions within this pattern
   *
   * @return an iterator over the subexpressions. Default implementation returns an empty sequence
   */
  def iterateSubExpressions(): Iterator[Expression] = Collections.EMPTY_LIST.iterator()

  /**
   * Allocate slots to any variables used within the pattern
   *
   * @param nextFree the next slot that is free to be allocated @return the next slot that is free to be allocated
   */
  def allocateSlots(nextFree: Int): Int = nextFree

  /**
   * If the pattern contains any calls on current(), this method is called to modify such calls
   * to become variable references to a variable declared in a specially-allocated local variable
   *
   * @param let      the expression that assigns the local variable. This returns a dummy result, and is executed
   *                 just before evaluating the pattern, to get the value of the context item into the variable.
   * @param offer    A PromotionOffer used to process the expressions and change the call on current() into
   *                 a variable reference
   * @param topLevel
   * @throws XPathException
   */
  def resolveCurrent(let: LetExpression, offer: PromotionOffer, topLevel: Boolean): Unit = {
  }

  /**
   * Offer promotion for subexpressions within this pattern. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   * <p/>
   * <p>Unlike the corresponding method on [[Expression]], this method does not return anything:
   * it can make internal changes to the pattern, but cannot return a different pattern. Only certain
   * kinds of promotion are applicable within a pattern: specifically, promotions affecting local
   * variable references within the pattern.
   *
   * @param offer  details of the offer, for example the offer to move
   *               expressions that don't depend on the context to an outer level in
   *               the containing expression
   * @param parent
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any error is detected
   */
  def promote(offer: PromotionOffer, parent: Expression): Unit = {
  }

  /**
   * Determine whether this Pattern matches the given Node. This is the main external interface
   * for matching patterns: it sets current() to the node being tested
   *
   *
   * @param node    The NodeInfo representing the Element or other node to be tested against the Pattern
   * @param context The dynamic context. Only relevant if the pattern
   *                uses variables, or contains calls on functions such as document() or key().
   * @return true if the node matches the Pattern, false otherwise. A dynamic error while matching the
   * pattern is treated as recoverable, and causes the result to be returned as false.
   */
  def matches(node: NodeInfo, context: XPathContext): Boolean

  /**
   * Determine whether this Pattern matches the given Node. This is an internal interface used
   * for matching sub-patterns; it does not alter the value of current(). The default implementation
   * is identical to matches().
   *
   *
   * @param node    The NodeInfo representing the Element or other node to be tested against the Pattern
   * @param anchor
   * @param context The dynamic context. Only relevant if the pattern
   *                uses variables, or contains calls on functions such as document() or key().
   * @return true if the node matches the Pattern, false otherwise. A dynamic error while matching the
   * pattern is treated as recoverable, and causes the result to be returned as false.
   */
  protected def internalMatches(node: NodeInfo, anchor: NodeInfo, context: XPathContext): Boolean = {
    matches(node, context)
  }

  /**
   * Select nodes in a document that match this Pattern.
   *
   * @param doc     the document node at the root of a tree
   * @param context the dynamic evaluation context
   * @return an iterator over the selected nodes in the document.
   */
  def selectNodes(doc: DocumentInfo, context: XPathContext): SequenceIterator = {
    val kind = getNodeKind
    kind match {
      case Type.DOCUMENT ⇒ if (matches(doc, context)) {
        SingletonIterator.makeIterator(doc)
      } else {
        EmptyIterator.getInstance
      }
      case Type.ATTRIBUTE ⇒ {
        val allElements = doc.iterateAxis(Axis.DESCENDANT, NodeKindTest.ELEMENT)
        val atts = new MappingFunction() {

          def map(item: Item): SequenceIterator = {
            return item.asInstanceOf[NodeInfo].iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
          }
        }
        val allAttributes = new MappingIterator(allElements, atts)
        val test = new ItemMappingFunction() {

          def mapItem(item: Item): Item = {
            if (matches(item.asInstanceOf[NodeInfo], context)) {
              return item
            } else {
              return null
            }
          }
        }
        new ItemMappingIterator(allAttributes, test)
      }
      case Type.ELEMENT | Type.COMMENT | Type.TEXT | Type.PROCESSING_INSTRUCTION ⇒ {
        val allDescendants = doc.iterateAxis(Axis.DESCENDANT, NodeKindTest.makeNodeKindTest(kind))
        val test = new ItemMappingFunction() {

          def mapItem(item: Item): Item = {
            if (matches(item.asInstanceOf[NodeInfo], context)) {
              return item
            } else {
              return null
            }
          }
        }
        new ItemMappingIterator(allDescendants, test)
      }
      case Type.NODE ⇒ {
        val allChildren = doc.iterateAxis(Axis.DESCENDANT, AnyNodeTest.getInstance)
        val attsOrSelf = new MappingFunction() {

          def map(item: Item): SequenceIterator = {
            return new PrependIterator(item.asInstanceOf[NodeInfo], item.asInstanceOf[NodeInfo].iterateAxis(Axis.ATTRIBUTE, 
              AnyNodeTest.getInstance))
          }
        }
        val attributesOrSelf = new MappingIterator(allChildren, attsOrSelf)
        val test = new ItemMappingFunction() {

          def mapItem(item: Item): Item = {
            if (matches(item.asInstanceOf[NodeInfo], context)) {
              return item
            } else {
              return null
            }
          }
        }
        new ItemMappingIterator(attributesOrSelf, test)
      }
      case Type.NAMESPACE ⇒ throw new UnsupportedOperationException("Patterns can't match namespace nodes")
      case _ ⇒ throw new UnsupportedOperationException("Unknown node kind")
    }
  }

  /**
   * Determine the types of nodes to which this pattern applies. Used for optimisation.
   * For patterns that match nodes of several types, return Type.NODE
   *
   * @return the type of node matched by this pattern. e.g. Type.ELEMENT or Type.TEXT
   */
  def getNodeKind(): Int = Type.NODE

  /**
   * Get a NodeTest that all the nodes matching this pattern must satisfy
   *
   * @return a NodeTest, as specific as possible, which all the matching nodes satisfy
   */
  def getNodeTest(): NodeTest

  /**
   * Determine the default priority to use if this pattern appears as a match pattern
   * for a template with no explicit priority attribute.
   *
   * @return the default priority for the pattern
   */
  def getDefaultPriority(): Double = 0.5

  def getLocation: String = {
    "pattern " + originalText + " in " + getSystemId
  }

  def getSourceLocator: SourceLocator = this

  /**
   * Get the original pattern text
   */
  override def toString: String = {
    if (originalText != null) {
      originalText
    } else {
      "pattern matching " + getNodeTest.toString
    }
  }
}

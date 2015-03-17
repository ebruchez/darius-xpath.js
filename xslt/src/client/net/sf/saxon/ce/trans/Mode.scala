// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.instruct.Template
import client.net.sf.saxon.ce.expr.sort.GenericSorter
import client.net.sf.saxon.ce.expr.sort.Sortable
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern._
import client.net.sf.saxon.ce.style.StylesheetModule
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.Whitespace
import com.google.gwt.core.client.JavaScriptObject
import java.util.ArrayList
import java.util.HashMap
import java.util.Iterator
import Mode._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object Mode {

  val UNNAMED_MODE = -1

  val NAMED_MODE = -3

  val ALL_MODES = new StructuredQName("saxon", NamespaceConstant.SAXON, "_omniMode")

  val UNNAMED_MODE_NAME = new StructuredQName("saxon", NamespaceConstant.SAXON, "_defaultMode")

  private def showPattern(p: Pattern): String = {
    Whitespace.collapseWhitespace(p.toString).toString
  }

  /**
   * Supporting class used at compile time to sort all the rules into precedence/priority
   * order and allocate a rank to each one, so that at run-time, comparing two rules to see
   * which has higher precedence/priority is a simple integer subtraction.
   */
  private class RuleSorter extends Sortable {

    var rules: ArrayList[Rule] = new ArrayList[Rule](100)

    def addRule(rule: Rule) {
      rules.add(rule)
    }

    def compare(a: Int, b: Int): Int = {
      rules.get(a).compareComputedRank(rules.get(b))
    }

    def swap(a: Int, b: Int) {
      val temp = rules.get(a)
      rules.set(a, rules.get(b))
      rules.set(b, temp)
    }

    def allocateRanks() {
      GenericSorter.quickSort(0, rules.size, this)
      var rank = 0
      for (i <- 0 until rules.size) {
        if (i > 0 && 
          rules.get(i - 1).compareComputedRank(rules.get(i)) != 0) {
          rank += 1
        }
        rules.get(i).setRank(rank)
      }
    }
  }

  /**
   * Interface for helper classes used to filter a chain of rules
   */
  private trait RuleFilter {

    /**
     * Test a rule to see whether it should be included
     * @param r the rule to be tested
     * @return true if the rule qualifies
     */
    def testRule(r: Rule): Boolean
  }

  /**
   * Interface for helper classes used to process all the rules in the Mode
   */
  private trait RuleAction {

    /**
     * Process a given rule
     * @param r the rule to be processed
     */
    def processRule(r: Rule): Unit
  }
}

/**
 * A Mode is a collection of rules; the selection of a rule to apply to a given element
 * is determined by a Pattern.
 *
 * @author Michael H. Kay
 */
class Mode(usage: Int, @BeanProperty var modeName: StructuredQName) {

  private var genericNodeRuleChain: Rule = null

  private var virtualRuleChain: ArrayList[Rule] = null

  private var documentRuleChain: Rule = null

  private var textRuleChain: Rule = null

  private var commentRuleChain: Rule = null

  private var processingInstructionRuleChain: Rule = null

  private var namespaceRuleChain: Rule = null

  private var unnamedElementRuleChain: Rule = null

  private var unnamedAttributeRuleChain: Rule = null

  private var namedElementRuleChains: HashMap[StructuredQName, Rule] = new HashMap[StructuredQName, Rule](32)

  private var namedAttributeRuleChains: HashMap[StructuredQName, Rule] = new HashMap[StructuredQName, Rule](8)

  private var mostRecentRule: Rule = _

  private var mostRecentModuleHash: Int = _

  private var isDefault: Boolean = (usage == UNNAMED_MODE)

  private var hasRules: Boolean = false

  private var stackFrameSlotsNeeded: Int = 0

  /**
   * Construct a new Mode, copying the contents of an existing Mode
   *
   * @param omniMode the existing mode. May be null, in which case it is not copied
   * @param modeName the name of the new mode to be created
   */
  def this(omniMode: Mode, modeName: StructuredQName) {
    this()
    isDefault = false
    this.modeName = modeName
    if (omniMode != null) {
      documentRuleChain = if (omniMode.documentRuleChain == null) null else new Rule(omniMode.documentRuleChain)
      textRuleChain = if (omniMode.textRuleChain == null) null else new Rule(omniMode.textRuleChain)
      commentRuleChain = if (omniMode.commentRuleChain == null) null else new Rule(omniMode.commentRuleChain)
      processingInstructionRuleChain = if (omniMode.processingInstructionRuleChain == null) null else new Rule(omniMode.processingInstructionRuleChain)
      namespaceRuleChain = if (omniMode.namespaceRuleChain == null) null else new Rule(omniMode.namespaceRuleChain)
      unnamedElementRuleChain = if (omniMode.unnamedElementRuleChain == null) null else new Rule(omniMode.unnamedElementRuleChain)
      unnamedAttributeRuleChain = if (omniMode.unnamedAttributeRuleChain == null) null else new Rule(omniMode.unnamedAttributeRuleChain)
      namedElementRuleChains = new HashMap[StructuredQName, Rule](omniMode.namedElementRuleChains.size)
      var ii = omniMode.namedElementRuleChains.keySet.iterator()
      while (ii.hasNext) {
        val fp = ii.next()
        val r = omniMode.namedElementRuleChains.get(fp)
        namedElementRuleChains.put(fp, new Rule(r))
      }
      ii = omniMode.namedAttributeRuleChains.keySet.iterator()
      while (ii.hasNext) {
        val fp = ii.next()
        val r = omniMode.namedAttributeRuleChains.get(fp)
        namedAttributeRuleChains.put(fp, new Rule(r))
      }
      mostRecentRule = omniMode.mostRecentRule
      mostRecentModuleHash = omniMode.mostRecentModuleHash
    }
  }

  def getVirtualRuleSet(): ArrayList[Rule] = virtualRuleChain

  /**
   * Determine if this is the default mode
   * @return true if this is the default (unnamed) mode
   */
  def isDefaultMode(): Boolean = isDefault

  /**
   * Ask whether there are any template rules in this mode
   * (a mode could exist merely because it is referenced in apply-templates)
   * @return true if no template rules exist in this mode
   */
  def isEmpty(): Boolean = !hasRules

  /**
   * Add a rule to the Mode.
   *
   * @param pattern          a Pattern
   * @param action     the Object to return from getRule() when the supplied node matches this Pattern
   * @param module the stylesheet module containing the rule
   * @param explicitMode  true if adding a template rule for a specific (default or named) mode;
   *      false if adding a rule because it applies to all modes
   */
  def addRule(pattern: Pattern, 
      action: Template, 
      module: StylesheetModule, 
      priority: Double, 
      explicitMode: Boolean, 
      ixslPreventDefault: Boolean, 
      ixslEventProperty: String) {
    if (explicitMode) {
      hasRules = true
    }
    if (pattern.getNodeTest.isInstanceOf[EmptySequenceTest]) {
      return
    }
    val moduleHash = module.hashCode
    var sequence: Int = 0
    sequence = if (mostRecentRule == null) 0 else if (action == mostRecentRule.getAction && moduleHash == mostRecentModuleHash) mostRecentRule.getSequence else mostRecentRule.getSequence + 1
    val precedence = module.getPrecedence
    val minImportPrecedence = module.getMinImportPrecedence
    val newRule = new Rule(pattern, action, precedence, minImportPrecedence, priority, sequence, ixslPreventDefault, 
      ixslEventProperty)
    if (pattern.isInstanceOf[NodeTestPattern]) {
      val test = pattern.getNodeTest
      if (test.isInstanceOf[AnyNodeTest]) {
        newRule.setAlwaysMatches(true)
      } else if (test.isInstanceOf[NodeKindTest]) {
        newRule.setAlwaysMatches(true)
      } else if (test.isInstanceOf[NameTest]) {
        val kind = test.getRequiredNodeKind
        if (kind == Type.ELEMENT || kind == Type.ATTRIBUTE) {
          newRule.setAlwaysMatches(true)
        }
      }
    }
    mostRecentRule = newRule
    mostRecentModuleHash = moduleHash
    val kind = pattern.getNodeKind
    val nodeName = (if (pattern.getNodeTest.isInstanceOf[NameTest]) pattern.getNodeTest.asInstanceOf[NameTest].getRequiredNodeName else null)
    kind match {
      case Type.ELEMENT => {
        if (nodeName == null) {
          unnamedElementRuleChain = addRuleToList(newRule, unnamedElementRuleChain)
        } else {
          val chain = namedElementRuleChains.get(nodeName)
          namedElementRuleChains.put(nodeName, addRuleToList(newRule, chain))
        }
        //break
      }
      case Type.ATTRIBUTE => {
        if (nodeName == null) {
          unnamedAttributeRuleChain = addRuleToList(newRule, unnamedAttributeRuleChain)
        } else {
          val chain = namedAttributeRuleChains.get(nodeName)
          namedAttributeRuleChains.put(nodeName, addRuleToList(newRule, chain))
        }
        //break
      }
      case Type.NODE => genericNodeRuleChain = addRuleToList(newRule, genericNodeRuleChain)
      case Type.DOCUMENT => documentRuleChain = addRuleToList(newRule, documentRuleChain)
      case Type.TEXT => textRuleChain = addRuleToList(newRule, textRuleChain)
      case Type.COMMENT => commentRuleChain = addRuleToList(newRule, commentRuleChain)
      case Type.PROCESSING_INSTRUCTION => processingInstructionRuleChain = addRuleToList(newRule, processingInstructionRuleChain)
      case Type.NAMESPACE => namespaceRuleChain = addRuleToList(newRule, namespaceRuleChain)
      case Type.EMPTY => if (pattern.isInstanceOf[JSObjectPattern]) {
        if (virtualRuleChain == null) {
          virtualRuleChain = new ArrayList[Rule]()
        }
        newRule.setIsVirtual()
        virtualRuleChain.add(newRule)
      }
    }
  }

  /**
   * Insert a new rule into this list before others of the same precedence/priority
   * @param newRule the new rule to be added into the list
   * @param list the Rule at the head of the list, or null if the list is empty
   * @return the new head of the list (which might be the old head, or the new rule if it
   * was inserted at the start)
   */
  private def addRuleToList(newRule: Rule, list: Rule): Rule = {
    if (list == null) {
      return newRule
    }
    val precedence = newRule.getPrecedence
    val priority = newRule.getPriority
    var rule = list
    var prev: Rule = null
    while (rule != null) {
      if ((rule.getPrecedence < precedence) || 
        (rule.getPrecedence == precedence && rule.getPriority <= priority)) {
        newRule.setNext(rule)
        if (prev == null) {
          return newRule
        } else {
          prev.setNext(newRule)
        }
        //break
      } else {
        prev = rule
        rule = rule.getNext
      }
    }
    if (rule == null) {
      prev.setNext(newRule)
      newRule.setNext(null)
    }
    list
  }

  /**
   * Specify how many slots for local variables are required by a particular pattern
   * @param slots the number of slots needed
   */
  def allocatePatternSlots(slots: Int) {
    stackFrameSlotsNeeded = Math.max(stackFrameSlotsNeeded, slots)
  }

  /**
   * Make a new XPath context for evaluating patterns if there is any possibility that the
   * pattern uses local variables
   *
   * @param context The existing XPath context
   * @return a new XPath context (or the existing context if no new context was created)
   */
  private def makeNewContext(context: XPathContext): XPathContext = {
    context = context.newContext()
    context.openStackFrame(stackFrameSlotsNeeded)
    context
  }

  def getVirtualRule(context: XPathContext): Rule = {
    if (virtualRuleChain == null) {
      return null
    }
    val eventObject = context.getController.getUserData("Saxon-CE", "current-object").asInstanceOf[JavaScriptObject]
    for (r <- virtualRuleChain) {
      val jso = r.getPattern.asInstanceOf[JSObjectPattern]
      if (jso.matchesObject(eventObject)) {
        return r
      }
    }
    null
  }

  /**
   * Get the rule corresponding to a given Node, by finding the best Pattern match.
   *
   * @param node the NodeInfo referring to the node to be matched
   * @param context the XPath dynamic evaluation context
   * @return the best matching rule, if any (otherwise null).
   */
  def getRule(node: NodeInfo, context: XPathContext): Rule = {
    if (stackFrameSlotsNeeded > 0) {
      context = makeNewContext(context)
    }
    var unnamedNodeChain: Rule = null
    var bestRule: Rule = null
    node.getNodeKind match {
      case Type.DOCUMENT => unnamedNodeChain = documentRuleChain
      case Type.ELEMENT => {
        unnamedNodeChain = unnamedElementRuleChain
        val namedNodeChain = namedElementRuleChains.get(node.getNodeName)
        if (namedNodeChain != null) {
          bestRule = searchRuleChain(node, context, null, namedNodeChain)
        }
        //break
      }
      case Type.ATTRIBUTE => {
        unnamedNodeChain = unnamedAttributeRuleChain
        val namedNodeChain = namedAttributeRuleChains.get(node.getNodeName)
        if (namedNodeChain != null) {
          bestRule = searchRuleChain(node, context, null, namedNodeChain)
        }
        //break
      }
      case Type.TEXT => unnamedNodeChain = textRuleChain
      case Type.COMMENT => unnamedNodeChain = commentRuleChain
      case Type.PROCESSING_INSTRUCTION => unnamedNodeChain = processingInstructionRuleChain
      case Type.NAMESPACE => unnamedNodeChain = namespaceRuleChain
      case _ => throw new AssertionError("Unknown node kind")
    }
    if (unnamedNodeChain != null) {
      bestRule = searchRuleChain(node, context, bestRule, unnamedNodeChain)
    }
    if (genericNodeRuleChain != null) {
      bestRule = searchRuleChain(node, context, bestRule, genericNodeRuleChain)
    }
    bestRule
  }

  /**
   * Search a chain of rules
   * @param node the node being matched
   * @param context XPath dynamic context
   * @param bestRule the best rule so far in terms of precedence and priority (may be null)
   * @param head the rule at the head of the chain to be searched
   * @return the best match rule found in the chain, or the previous best rule, or null
   * @throws XPathException
   */
  private def searchRuleChain(node: NodeInfo, 
      context: XPathContext, 
      bestRule: Rule, 
      head: Rule): Rule = {
    while (head != null) {
      if (bestRule != null) {
        val rank = head.compareRank(bestRule)
        if (rank < 0) {
          //break
        } else if (rank == 0) {
          if (head.isAlwaysMatches || head.getPattern.matches(node, context)) {
            bestRule = (if (bestRule.getSequence > head.getSequence) bestRule else head)
            //break
          } else {
          }
        } else {
          if (head.isAlwaysMatches || head.getPattern.matches(node, context)) {
            bestRule = head
          }
        }
      } else if (head.isAlwaysMatches || head.getPattern.matches(node, context)) {
        bestRule = head
        //break
      }
      head = head.getNext
    }
    bestRule
  }

  /**
   * Get the rule corresponding to a given Node, by finding the best Pattern match.
   *
   * @param node the NodeInfo referring to the node to be matched
   * @param context the XPath dynamic evaluation context
   * @return the best matching rule, if any (otherwise null).
   */
  def getRule(node: NodeInfo, context: XPathContext, filter: RuleFilter): Rule = {
    if (stackFrameSlotsNeeded > 0) {
      context = makeNewContext(context)
    }
    var bestRule: Rule = null
    var unnamedNodeChain: Rule = null
    node.getNodeKind match {
      case Type.DOCUMENT => unnamedNodeChain = documentRuleChain
      case Type.ELEMENT => {
        unnamedNodeChain = unnamedElementRuleChain
        val namedNodeChain = namedElementRuleChains.get(node.getNodeName)
        bestRule = searchRuleChain(node, context, null, namedNodeChain, filter)
        //break
      }
      case Type.ATTRIBUTE => {
        unnamedNodeChain = unnamedAttributeRuleChain
        val namedNodeChain = namedAttributeRuleChains.get(node.getNodeName)
        bestRule = searchRuleChain(node, context, null, namedNodeChain, filter)
        //break
      }
      case Type.TEXT => unnamedNodeChain = textRuleChain
      case Type.COMMENT => unnamedNodeChain = commentRuleChain
      case Type.PROCESSING_INSTRUCTION => unnamedNodeChain = processingInstructionRuleChain
      case Type.NAMESPACE => unnamedNodeChain = namespaceRuleChain
      case _ => throw new AssertionError("Unknown node kind")
    }
    bestRule = searchRuleChain(node, context, bestRule, unnamedNodeChain, filter)
    searchRuleChain(node, context, bestRule, genericNodeRuleChain, filter)
  }

  /**
   * Search a chain of rules
   * @param node the node being matched
   * @param context XPath dynamic context
   * @param bestRule the best rule so far in terms of precedence and priority (may be null)
   * @param head the rule at the head of the chain to be searched
   * @return the best match rule found in the chain, or the previous best rule, or null
   * @throws XPathException
   */
  private def searchRuleChain(node: NodeInfo, 
      context: XPathContext, 
      bestRule: Rule, 
      head: Rule, 
      filter: RuleFilter): Rule = {
    while (head != null) {
      if (filter.testRule(head)) {
        if (bestRule != null) {
          val rank = head.compareRank(bestRule)
          if (rank < 0) {
            //break
          } else if (rank == 0) {
            if (head.isAlwaysMatches || head.getPattern.matches(node, context)) {
              bestRule = (if (bestRule.getSequence > head.getSequence) bestRule else head)
              //break
            } else {
            }
          } else {
            if (head.isAlwaysMatches || head.getPattern.matches(node, context)) {
              bestRule = head
            }
          }
        } else if (head.isAlwaysMatches || head.getPattern.matches(node, context)) {
          bestRule = head
          //break
        }
      }
      head = head.getNext
    }
    bestRule
  }

  /**
   * Get the rule corresponding to a given Node, by finding the best Pattern match, subject to a minimum
   * and maximum precedence. (This supports xsl:apply-imports)
   *
   * @param node the NodeInfo referring to the node to be matched
   * @param min the minimum import precedence
   * @param max the maximum import precedence
   * @param context the XPath dynamic evaluation context
   * @return the Rule registered for that node, if any (otherwise null).
   */
  def getRule(node: NodeInfo, 
      min: Int, 
      max: Int, 
      context: XPathContext): Rule = {
    val filter = new RuleFilter() {

      def testRule(r: Rule): Boolean = {
        var p = r.getPrecedence
        return p >= min && p <= max
      }
    }
    getRule(node, context, filter)
  }

  /**
   * Get the rule corresponding to a given Node, by finding the next-best Pattern match
   * after the specified object.
   *
   * @param node the NodeInfo referring to the node to be matched
   * @param currentRule the current rule; we are looking for the next match after the current rule
   * @param context the XPath dynamic evaluation context
   * @return the object (e.g. a NodeHandler) registered for that element, if any (otherwise null).
   */
  def getNextMatchRule(node: NodeInfo, currentRule: Rule, context: XPathContext): Rule = {
    val filter = new RuleFilter() {

      def testRule(r: Rule): Boolean = {
        var comp = r.compareRank(currentRule)
        return comp < 0 || 
          (comp == 0 && r.getSequence < currentRule.getSequence)
      }
    }
    getRule(node, context, filter)
  }

  /**
   * Walk over all the rules, applying a specified action to each one.
   * @param action an action that is to be applied to all the rules in this Mode
   */
  def processRules(action: RuleAction) {
    processRuleChain(documentRuleChain, action)
    processRuleChain(unnamedElementRuleChain, action)
    var ii = namedElementRuleChains.keySet.iterator()
    while (ii.hasNext) {
      val r = namedElementRuleChains.get(ii.next())
      processRuleChain(r, action)
    }
    processRuleChain(unnamedAttributeRuleChain, action)
    ii = namedAttributeRuleChains.keySet.iterator()
    while (ii.hasNext) {
      val r = namedAttributeRuleChains.get(ii.next())
      processRuleChain(r, action)
    }
    processRuleChain(textRuleChain, action)
    processRuleChain(commentRuleChain, action)
    processRuleChain(processingInstructionRuleChain, action)
    processRuleChain(namespaceRuleChain, action)
    processRuleChain(genericNodeRuleChain, action)
  }

  private def processRuleChain(r: Rule, action: RuleAction) {
    while (r != null) {
      action.processRule(r)
      r = r.getNext
    }
  }

  /**
   * Compute a rank for each rule, as a combination of the precedence and priority, to allow
   * rapid comparison.
   */
  def computeRankings() {
    val sorter = new RuleSorter()
    val addToSorter = new RuleAction() {

      def processRule(r: Rule) {
        sorter.addRule(r)
      }
    }
    processRules(addToSorter)
    sorter.allocateRanks()
  }
}

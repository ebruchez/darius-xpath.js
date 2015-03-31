// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.expr.instruct.Template
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern._
import client.net.sf.saxon.ce.style.StylesheetModule
import client.net.sf.saxon.ce.`type`.Type
import java.util.HashMap
import StripSpaceRules._
//remove if not needed
import scala.collection.JavaConversions._

object StripSpaceRules {

  val STRIP = new Template()

  val PRESERVE = new Template()
}

/**
 * The set of rules used to decide strip-space/preserve-space matching of element names in XSLT.
 *
 * @author Michael H. Kay
 */
class StripSpaceRules {

  private var anyElementRule: Rule = null

  private var unnamedElementRuleChain: Rule = null

  private var namedElementRules: HashMap[StructuredQName, Rule] = new HashMap[StructuredQName, Rule](32)

  private var sequence: Int = 0

  var isStripping: Boolean = false

  /**
   * Add a rule
   *
   * @param test          a NodeTest (*, *:local, prefix:*, or QName)
   * @param action           StripRuleTarget.STRIP or StripRuleTarget.PRESERVE
   * @param module the stylesheet module containing the rule
   */
  def addRule(test: NodeTest, action: Template, module: StylesheetModule): Unit = {
    val precedence = module.getPrecedence
    val minImportPrecedence = module.getMinImportPrecedence
    val priority = test.getDefaultPriority
    val pattern = new NodeTestPattern(test)
    pattern.setSystemId(module.getSourceElement.getSystemId)
    val newRule = new Rule(pattern, action, precedence, minImportPrecedence, priority, sequence += 1, 
      false, null)
    newRule.setRank((precedence << 16) + sequence)
    if (test.isInstanceOf[NodeKindTest]) {
      newRule.setAlwaysMatches(true)
      anyElementRule = addRuleToList(newRule, anyElementRule, true)
    } else if (test.isInstanceOf[NameTest]) {
      newRule.setAlwaysMatches(true)
      val fp = test.asInstanceOf[NameTest].getRequiredNodeName
      val chain = namedElementRules.get(fp)
      namedElementRules.put(fp, addRuleToList(newRule, chain, true))
    } else {
      unnamedElementRuleChain = addRuleToList(newRule, unnamedElementRuleChain, false)
    }
    if (action == STRIP) {
      isStripping = true
    }
  }

  /**
   * Insert a new rule into this list before others of the same precedence
   * (we rely on the fact that all rules in a list have the same priority)
   * @param newRule the new rule to be added into the list
   * @param list the Rule at the head of the list, or null if the list is empty
   * @param dropRemainder if only one rule needs to be retained
   * @return the new head of the list (which might be the old head, or the new rule if it
   * was inserted at the start)
   */
  private def addRuleToList(newRule: Rule, list: Rule, dropRemainder: Boolean): Rule = {
    if (list == null) {
      return newRule
    }
    val precedence = newRule.getPrecedence
    var rule = list
    var prev: Rule = null
    while (rule != null) {
      if (rule.getPrecedence <= precedence) {
        newRule.setNext(if (dropRemainder) null else rule)
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

  def isSpaceStripped(elementName: StructuredQName): Boolean = {
    val rule = getRule(elementName)
    rule != null && rule.getAction == STRIP
  }

  /**
   * Get the rule corresponding to a given element node, by finding the best pattern match.
   *
   * @param fingerprint the name of the element node to be matched
   * @return the best matching rule, if any (otherwise null).
   */
  private def getRule(fingerprint: StructuredQName): Rule = {
    var bestRule = namedElementRules.get(fingerprint)
    if (unnamedElementRuleChain != null) {
      bestRule = searchRuleChain(fingerprint, bestRule, unnamedElementRuleChain)
    }
    if (anyElementRule != null) {
      bestRule = searchRuleChain(fingerprint, bestRule, anyElementRule)
    }
    bestRule
  }

  /**
   * Search a chain of rules
   * @param fingerprint the name of the element node being matched
   * @param bestRule the best rule so far in terms of precedence and priority (may be null)
   * @param head the rule at the head of the chain to be searched
   * @return the best match rule found in the chain, or the previous best rule, or null
   * @throws client.net.sf.saxon.ce.trans.XPathException
   */
  private def searchRuleChain(fingerprint: StructuredQName, bestRule: Rule, head: Rule): Rule = {
    while (head != null) {
      if (bestRule != null) {
        val rank = head.compareRank(bestRule)
        if (rank < 0) {
          //break
        } else if (rank == 0) {
          if (head.isAlwaysMatches || 
            head.getPattern.getNodeTest.matches(Type.ELEMENT, fingerprint)) {
            bestRule = head
            //break
          } else {
          }
        } else {
          if (head.isAlwaysMatches || 
            head.getPattern.getNodeTest.matches(Type.ELEMENT, fingerprint)) {
            bestRule = head
          }
        }
      } else if (head.isAlwaysMatches || 
        head.getPattern.getNodeTest.matches(Type.ELEMENT, fingerprint)) {
        bestRule = head
        //break
      }
      head = head.getNext
    }
    bestRule
  }
}
